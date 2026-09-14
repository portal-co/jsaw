// Hand-written WASI preview1 glue for jsaw's self-hosting dogfood.
// The compiler owns linear memory and binds it before invoking WASI imports.
// State follows jsaw's stable cross-function array / primitive surface.
let memory = [null, null]; // Uint8Array, DataView
// Reference-valued mutable bindings live in a top-level array slot. jsaw's
// context-property writer cannot safely store a function parameter, while
// array slots do preserve that value across imported calls.
let state = [[], 0, [], [], [], -1, 12345];
// slots: stdin, stdinPos, stdout, stderr, argv, exitCode, randomState

export function __wasi_bind_memory(mem) { memory[0] = mem; memory[1] = new DataView(mem.buffer); }
export function __wasi_set_stdin(bytes) { state[0] = bytes; state[1] = 0; }
export function __wasi_set_args(args) { state[4] = args; }
export function __wasi_take_stdout() { return state[2]; }
export function __wasi_take_stderr() { return state[3]; }
export function __wasi_exit_code() { return state[5]; }
export function __wasi_debug_stdin_first() { return Number(state[0][0]); }
export function __wasi_debug_write_literal() { memory[1].setUint8(32,42); }

export function fd_read(fd, iovs, count, nread) {
  let dv=memory[1], total=0, stop=false;
  for(let i=0;i<count&&!stop;i++) { let ptr=dv.getUint32(iovs+i*8,true), len=dv.getUint32(iovs+i*8+4,true);
    for(let j=0;j<len;j++) { if(fd!==0||state[1]>=state[0].length){stop=true;break;} dv.setUint8(ptr+j,Number(state[0][state[1]]));state[1]=state[1]+1;total=total+1; }
  } dv.setUint32(nread,total,true);return[0n];
}
export function fd_write(fd, iovs, count, nwritten) {
  let dv=memory[1], total=0, out=state[2];if(fd===2)out=state[3];
  for(let i=0;i<count;i++) { let ptr=dv.getUint32(iovs+i*8,true), len=dv.getUint32(iovs+i*8+4,true);for(let j=0;j<len;j++)out.push(dv.getUint8(ptr+j));total=total+len; }
  dv.setUint32(nwritten,total,true);return[0n];
}
export function fd_close(fd) { return[0n]; }
export function fd_fdstat_get(fd, ptr) { let dv=memory[1];dv.setUint8(ptr,2);dv.setUint16(ptr+2,0,true);dv.setBigUint64(ptr+8,0n,true);dv.setBigUint64(ptr+16,0n,true);return[0n]; }
export function fd_filestat_get(fd, ptr) { let dv=memory[1];for(let i=0;i<8;i++)dv.setBigUint64(ptr+i*8,0n,true);return[0n]; }
export function fd_prestat_get(fd, ptr) { let dv=memory[1];dv.setUint8(ptr,0);dv.setUint32(ptr+4,4,true);return[0n]; }
export function fd_prestat_dir_name(fd, ptr, len) { let x=[47,115,114,99];if(fd===4)x=[47,111,117,116];for(let i=0;i<4;i++)memory[1].setUint8(ptr+i,x[i]);return[0n]; }
export function path_open(fd,a,b,c,d,e,f,g,out) { memory[1].setUint32(out,0,true);return[44n]; }
export function path_create_directory(fd,ptr,len) { return[0n]; }
export function path_filestat_get(fd,a,b,c,ptr) { let dv=memory[1];for(let i=0;i<8;i++)dv.setBigUint64(ptr+i*8,0n,true);return[44n]; }
export function args_sizes_get(count,bytes) { let n=0;for(let i=0;i<state[4].length;i++)n=n+state[4][i].length+1;memory[1].setUint32(count,state[4].length,true);memory[1].setUint32(bytes,n,true);return[0n]; }
export function args_get(ptrs,bytes) { let p=bytes;for(let i=0;i<state[4].length;i++){memory[1].setUint32(ptrs+i*4,p,true);for(let j=0;j<state[4][i].length;j++){memory[1].setUint8(p,state[4][i][j]);p=p+1;}memory[1].setUint8(p,0);p=p+1;}return[0n]; }
export function environ_sizes_get(count,bytes) { memory[1].setUint32(count,0,true);memory[1].setUint32(bytes,0,true);return[0n]; }
export function environ_get(ptrs,bytes) { return[0n]; }
export function proc_exit(code) { state[5]=code;return[]; }
export function random_get(ptr,len) { let x=state[6],dv=memory[1];for(let i=0;i<len;i++){x=x^(x<<13);x=x^(x>>>17);x=x^(x<<5);dv.setUint8(ptr+i,x&255);}state[6]=x;return[0n]; }
