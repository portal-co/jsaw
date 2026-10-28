//! The jsaw MiniBC host runtime (DreamComp #39): the stateful host-op table
//! (`0x9B`) backing the MiniBC lane — nano arena, CoreGC handle heap with a
//! precise mark/sweep collector, module globals, and FFI dispatch.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use dreamcompiler_minibc::interp::MiniInterp;

use super::minibc_render::{MinibcArtifact, host_idx};
use super::{LeModule, LeType};

/// Per-layout physical info (same natural-alignment math as the C lane).
struct LayoutInfo {
    /// Byte size for structs; None for arrays.
    size: Option<usize>,
    /// Array element byte size.
    elem_size: usize,
    elem_is_ref: bool,
    /// Byte offset of the element payload.
    data_off: usize,
    /// Byte offsets of reference fields (structs).
    ref_offsets: Vec<usize>,
    /// Byte offsets of all struct fields.
    field_offsets: Vec<usize>,
    field_sizes: Vec<usize>,
    is_array: bool,
}

fn type_size(ty: LeType) -> usize {
    match ty {
        LeType::I8 => 1,
        LeType::I16 => 2,
        LeType::I32 | LeType::F32 => 4,
        LeType::I64 | LeType::F64 | LeType::Ref | LeType::Slot => 8,
    }
}

const HEADER_BYTES: usize = 16; // { type_id: u32, flags: u32, len: u32, pad: u32 }

/// Returns (by type_id for `COREGC_ALLOC`, by layout index for field/index
/// access — the IR's `layout` operand is a `LeModule::layouts` index).
fn layout_info(m: &LeModule) -> (BTreeMap<u32, LayoutInfo>, BTreeMap<u32, LayoutInfo>) {
    let mut by_idx = BTreeMap::new();
    let mut out = BTreeMap::new();
    for (idx, layout) in m.layouts.iter().enumerate().skip(1) {
        let mut off = HEADER_BYTES;
        let mut ref_offsets = Vec::new();
        match &layout.array_element {
            Some(elem) => {
                let esz = type_size(*elem);
                let align = esz;
                off = off.div_ceil(align) * align;
                let li = LayoutInfo {
                    size: None,
                    elem_size: esz,
                    elem_is_ref: *elem == LeType::Ref,
                    data_off: off,
                    ref_offsets: Vec::new(),
                    field_offsets: Vec::new(),
                    field_sizes: Vec::new(),
                    is_array: true,
                };
                out.insert(layout.type_id, li.clone_meta());
                by_idx.insert(idx as u32, li);
            }
            None => {
                let mut fields = Vec::new();
                let mut fsizes = Vec::new();
                for f in &layout.fields {
                    let sz = type_size(*f);
                    off = off.div_ceil(sz) * sz;
                    if *f == LeType::Ref {
                        ref_offsets.push(off);
                    }
                    fields.push(off);
                    fsizes.push(sz);
                    off += sz;
                }
                let li = LayoutInfo {
                    size: Some(off.max(HEADER_BYTES)),
                    elem_size: 0,
                    elem_is_ref: false,
                    data_off: 0,
                    ref_offsets,
                    field_offsets: fields,
                    field_sizes: fsizes,
                    is_array: false,
                };
                out.insert(layout.type_id, li.clone_meta());
                by_idx.insert(idx as u32, li);
            }
        }
    }
    (out, by_idx)
}

struct HostState {
    /// nano lane: the arena.
    arena: Vec<u8>,
    /// coregc lane: handle table (handle word = (index + 1) * 2).
    objects: Vec<Option<GcObj>>,
    free_handles: Vec<u32>,
    /// Root shadow stack: frames of slots.
    root_frames: Vec<Vec<u64>>,
    /// Module globals (raw words).
    globals: Vec<u64>,
    /// Layouts by type id (coregc).
    layouts: Rc<BTreeMap<u32, LayoutInfo>>,
    layouts_by_idx: Rc<BTreeMap<u32, LayoutInfo>>,
    /// Bytes allocated since the last collection.
    since_gc: usize,
    gc_threshold: usize,
}

struct GcObj {
    type_id: u32,
    marked: bool,
    /// Raw bytes (header + payload).
    data: Vec<u8>,
}

impl HostState {
    fn handle_of(&self, w: u64) -> Option<usize> {
        if w == 0 || w & 1 != 0 {
            return None;
        }
        let idx = (w / 2) as usize;
        idx.checked_sub(1).filter(|&i| i < self.objects.len())
    }

    fn obj(&self, w: u64) -> &GcObj {
        let i = self.handle_of(w).expect("dangling coregc handle");
        self.objects[i].as_ref().expect("dangling coregc handle")
    }

    fn obj_mut(&mut self, w: u64) -> &mut GcObj {
        let i = self.handle_of(w).expect("dangling coregc handle");
        self.objects[i].as_mut().expect("dangling coregc handle")
    }

    fn collect(&mut self) {
        // mark
        let mut work: Vec<u64> = self
            .root_frames
            .iter()
            .flat_map(|f| f.iter().copied())
            .collect();
        while let Some(w) = work.pop() {
            let Some(i) = self.handle_of(w) else { continue };
            let Some(obj) = &mut self.objects[i] else { continue };
            if obj.marked {
                continue;
            }
            obj.marked = true;
            let li = &self.layouts[&obj.type_id];
            let data = &self.objects[i].as_ref().unwrap().data;
            if li.is_array {
                if li.elem_is_ref {
                    let len = u32::from_le_bytes(data[8..12].try_into().unwrap()) as usize;
                    for k in 0..len {
                        let off = li.data_off + k * li.elem_size;
                        work.push(u64::from_le_bytes(data[off..off + 8].try_into().unwrap()));
                    }
                }
            } else {
                for &ro in &li.ref_offsets {
                    work.push(u64::from_le_bytes(data[ro..ro + 8].try_into().unwrap()));
                }
            }
        }
        // sweep
        for (i, slot) in self.objects.iter_mut().enumerate() {
            match slot {
                Some(o) if o.marked => o.marked = false,
                Some(_) => {
                    *slot = None;
                    self.free_handles.push(i as u32);
                }
                None => {}
            }
        }
        self.since_gc = 0;
    }
}

/// The jsaw MiniBC host: owns the host-op table and shared runtime state.
pub struct JsawHost {
    state: Rc<RefCell<HostState>>,
    handlers: Vec<Box<dyn FnMut(&mut MiniInterp, &[u64]) -> u64>>,
}

impl JsawHost {
    /// Build the host-op table for an artifact's module.
    pub fn new(artifact: &MinibcArtifact, m: &LeModule) -> Self {
        let (layouts, by_idx) = layout_info(m);
        let layouts_by_idx = Rc::new(by_idx);
        let layouts = Rc::new(layouts);
        let state = Rc::new(RefCell::new(HostState {
            arena: Vec::new(),
            objects: Vec::new(),
            free_handles: Vec::new(),
            root_frames: Vec::new(),
            globals: m.globals.iter().map(|(_, init)| init.unwrap_or(0)).collect(),
            layouts,
            layouts_by_idx,
            since_gc: 0,
            gc_threshold: 1 << 20,
        }));
        let host = JsawHost {
            state,
            handlers: Vec::new(),
        };
        // Register handlers in `host_idx` order; each closure clones the
        // shared state handle.
        let mut handlers: Vec<Box<dyn FnMut(&mut MiniInterp, &[u64]) -> u64>> = Vec::new();

        macro_rules! h {
            ($f:expr) => {{
                let st = Rc::clone(&host.state);
                let f = $f;
                handlers.push(Box::new(move |_interp, args| f(&mut st.borrow_mut(), args)));
            }};
        }

        // NANO_ARENA_ENSURE
        h!(|s: &mut HostState, args: &[u64]| {
            let end = args[0] as usize;
            if s.arena.len() < end {
                s.arena.resize(end, 0);
            }
            0
        });
        // NANO_LOAD
        h!(|s: &mut HostState, args: &[u64]| {
            let addr = args[0] as usize;
            let n = 1usize << (args[1] as u32);
            let mut b = [0u8; 8];
            b[..n].copy_from_slice(&s.arena[addr..addr + n]);
            u64::from_le_bytes(b)
        });
        // NANO_STORE
        h!(|s: &mut HostState, args: &[u64]| {
            let addr = args[0] as usize;
            let n = 1usize << (args[1] as u32);
            s.arena[addr..addr + n].copy_from_slice(&args[2].to_le_bytes()[..n]);
            0
        });
        // COREGC_ALLOC
        h!(|s: &mut HostState, args: &[u64]| {
            let type_id = args[0] as u32;
            let len = args[1] as u32;
            let li = s.layouts.get(&type_id).expect("alloc: unknown type id").clone_meta();
            let size = if li.is_array {
                li.data_off + len as usize * li.elem_size
            } else {
                li.size.expect("struct size")
            };
            s.since_gc += size;
            if s.since_gc > s.gc_threshold {
                s.collect();
            }
            let mut data = vec![0u8; size];
            data[0..4].copy_from_slice(&type_id.to_le_bytes());
            data[8..12].copy_from_slice(&len.to_le_bytes());
            let obj = GcObj {
                type_id,
                marked: false,
                data,
            };
            let idx = if let Some(free) = s.free_handles.pop() {
                s.objects[free as usize] = Some(obj);
                free
            } else {
                s.objects.push(Some(obj));
                (s.objects.len() - 1) as u32
            };
            ((idx as u64) + 1) * 2
        });
        // COREGC_FIELD_READ
        h!(|s: &mut HostState, args: &[u64]| {
            let (off, n) = field_offset(&s.layouts_by_idx, args[1] as u32, args[2] as u32);
            let obj = s.obj(args[0]);
            read_sized(&obj.data, off, n)
        });
        // COREGC_FIELD_WRITE
        h!(|s: &mut HostState, args: &[u64]| {
            let (off, n) = field_offset(&s.layouts_by_idx, args[1] as u32, args[2] as u32);
            let obj = s.obj_mut(args[0]);
            write_sized(&mut obj.data, off, args[3], n);
            0
        });
        // COREGC_INDEX_READ
        h!(|s: &mut HostState, args: &[u64]| {
            let obj = s.obj(args[0]);
            let li = s.layouts_by_idx[&(args[1] as u32)].clone_meta();
            let off = li.data_off + args[2] as usize * li.elem_size;
            read_sized(&obj.data, off, li.elem_size)
        });
        // COREGC_INDEX_WRITE
        h!(|s: &mut HostState, args: &[u64]| {
            let li = s.layouts_by_idx[&(args[1] as u32)].clone_meta();
            let off = li.data_off + args[2] as usize * li.elem_size;
            let obj = s.obj_mut(args[0]);
            write_sized(&mut obj.data, off, args[3], li.elem_size);
            0
        });
        // COREGC_ROOT_ENTER
        h!(|s: &mut HostState, args: &[u64]| {
            s.root_frames.push(vec![0; args[0] as usize]);
            0
        });
        // COREGC_ROOT_STORE
        h!(|s: &mut HostState, args: &[u64]| {
            let frame = s.root_frames.last_mut().expect("root frame");
            frame[args[0] as usize] = args[1];
            0
        });
        // COREGC_ROOT_POP
        h!(|s: &mut HostState, _args: &[u64]| {
            s.root_frames.pop().expect("root pop");
            0
        });
        // COREGC_ARRAY_COPY
        h!(|s: &mut HostState, args: &[u64]| {
            let (d, di, src, si, len, log2) =
                (args[0], args[1] as usize, args[2], args[3] as usize, args[4] as usize, args[5] as usize);
            let li = s.layouts[&s.obj(d).type_id].clone_meta();
            let (d_off, s_off) = (li.data_off + di * li.elem_size, li.data_off + si * li.elem_size);
            let nbytes = len * li.elem_size;
            let _ = log2;
            // read source range first (handles overlap)
            let bytes: Vec<u8> = s.obj(src).data[s_off..s_off + nbytes].to_vec();
            s.obj_mut(d).data[d_off..d_off + nbytes].copy_from_slice(&bytes);
            0
        });
        // GLOBAL_GET
        h!(|s: &mut HostState, args: &[u64]| s.globals[args[0] as usize]);
        // GLOBAL_SET
        h!(|s: &mut HostState, args: &[u64]| {
            s.globals[args[0] as usize] = args[1];
            0
        });
        // FFI_CALL — no host imports in the corpus; fail closed.
        h!(|_s: &mut HostState, args: &[u64]| {
            panic!("jsaw minibc host: FFI_CALL ordinal {} with no host handler", args[0]);
        });

        let _ = artifact;
        JsawHost {
            state: host.state,
            handlers,
        }
    }

    /// Install this host's handlers into the interpreter.
    pub fn install(self, interp: &mut MiniInterp) {
        interp.host_ops = self.handlers;
    }
}

fn read_word(data: &[u8], off: usize) -> u64 {
    read_sized(data, off, 8)
}

fn read_sized(data: &[u8], off: usize, n: usize) -> u64 {
    let mut b = [0u8; 8];
    b[..n].copy_from_slice(&data[off..off + n]);
    u64::from_le_bytes(b)
}

fn write_word(data: &mut [u8], off: usize, v: u64) {
    write_sized(data, off, v, 8)
}

fn write_sized(data: &mut [u8], off: usize, v: u64, n: usize) {
    data[off..off + n].copy_from_slice(&v.to_le_bytes()[..n]);
}

/// Returns (byte offset, byte size). `layout` is a `LeModule::layouts`
/// index; layout 0 is the 16-byte allocation header (four u32 words).
fn field_offset(layouts: &BTreeMap<u32, LayoutInfo>, layout: u32, field: u32) -> (usize, usize) {
    if layout == 0 {
        return (field as usize * 4, 4);
    }
    let li = &layouts[&layout];
    (li.field_offsets[field as usize], li.field_sizes[field as usize])
}

impl LayoutInfo {
    fn clone_meta(&self) -> LayoutInfo {
        LayoutInfo {
            size: self.size,
            elem_size: self.elem_size,
            elem_is_ref: self.elem_is_ref,
            data_off: self.data_off,
            ref_offsets: self.ref_offsets.clone(),
            field_offsets: self.field_offsets.clone(),
            field_sizes: self.field_sizes.clone(),
            is_array: self.is_array,
        }
    }
}
