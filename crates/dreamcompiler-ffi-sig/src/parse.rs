//! Signature string parser.

use crate::types::{FfiAbiTy, FfiCallableSig, FfiMethodSig, FfiPitTy, FfiTy, PitRid};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("empty signature")]
    Empty,
    #[error("expected '(' at start of callable")]
    ExpectedOpenParen,
    #[error("expected ')' after arguments")]
    ExpectedCloseParen,
    #[error("expected '->'")]
    ExpectedArrow,
    #[error("expected type")]
    ExpectedType,
    #[error("invalid PIT rid hex: {0}")]
    InvalidPitRid(String),
    #[error("unknown ABI type `{0}`")]
    UnknownAbiType(String),
    #[error("trailing garbage: `{0}`")]
    TrailingGarbage(String),
    #[error("invalid method name")]
    InvalidMethodName,
}

fn parse_pit_rid(hex: &str) -> Result<PitRid, ParseError> {
    if hex.len() != 64 || !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(ParseError::InvalidPitRid(hex.to_string()));
    }
    let mut rid = [0u8; 32];
    for (i, chunk) in hex.as_bytes().chunks(2).enumerate() {
        let s = std::str::from_utf8(chunk).unwrap();
        rid[i] =
            u8::from_str_radix(s, 16).map_err(|_| ParseError::InvalidPitRid(hex.to_string()))?;
    }
    Ok(rid)
}

/// Parse a hybrid FFI signature string.
pub fn parse_sig(input: &str) -> Result<FfiCallableSig, ParseError> {
    let s = input.trim();
    if s.is_empty() {
        return Err(ParseError::Empty);
    }
    let mut p = Parser { s, i: 0 };
    let args = p.parse_arg_list()?;
    p.expect_str("->")?;
    let ret = p.parse_ty()?;
    let mut methods = Vec::new();
    loop {
        p.skip_ws();
        if p.peek() != Some(';') {
            break;
        }
        p.i += 1;
        methods.push(p.parse_method()?);
    }
    p.skip_ws();
    if p.i < p.s.len() {
        return Err(ParseError::TrailingGarbage(p.s[p.i..].to_string()));
    }
    Ok(FfiCallableSig { args, ret, methods })
}

struct Parser<'a> {
    s: &'a str,
    i: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<char> {
        self.s[self.i..].chars().next()
    }

    fn skip_ws(&mut self) -> bool {
        let start = self.i;
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            self.i += 1;
        }
        self.i != start
    }

    fn expect_str(&mut self, lit: &str) -> Result<(), ParseError> {
        self.skip_ws();
        if self.s[self.i..].starts_with(lit) {
            self.i += lit.len();
            Ok(())
        } else {
            Err(ParseError::ExpectedArrow)
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<FfiTy>, ParseError> {
        self.skip_ws();
        if self.peek() != Some('(') {
            return Err(ParseError::ExpectedOpenParen);
        }
        self.i += 1;
        self.skip_ws();
        let mut args = Vec::new();
        if self.peek() == Some(')') {
            self.i += 1;
            return Ok(args);
        }
        loop {
            args.push(self.parse_ty()?);
            self.skip_ws();
            match self.peek() {
                Some(',') => {
                    self.i += 1;
                }
                Some(')') => {
                    self.i += 1;
                    break;
                }
                _ => return Err(ParseError::ExpectedCloseParen),
            }
        }
        Ok(args)
    }

    fn parse_method(&mut self) -> Result<FfiMethodSig, ParseError> {
        self.skip_ws();
        let name_start = self.i;
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.i += 1;
        }
        if self.i == name_start {
            return Err(ParseError::InvalidMethodName);
        }
        let name = self.s[name_start..self.i].to_string();
        let args = self.parse_arg_list()?;
        self.expect_str("->")?;
        let ret = self.parse_ty()?;
        Ok(FfiMethodSig { name, args, ret })
    }

    fn parse_ty(&mut self) -> Result<FfiTy, ParseError> {
        self.skip_ws();
        if self.s[self.i..].starts_with("P<") {
            self.i += 2;
            let start = self.i;
            while matches!(self.peek(), Some(c) if c.is_ascii_hexdigit()) {
                self.i += 1;
            }
            let hex = &self.s[start..self.i];
            if self.peek() != Some('>') {
                return Err(ParseError::ExpectedType);
            }
            self.i += 1;
            let rid = parse_pit_rid(hex)?;
            return Ok(FfiTy::Pit(FfiPitTy { rid }));
        }
        let start = self.i;
        // Type words may contain digits (`i32`); stop at delimiters.
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.i += 1;
        }
        if start == self.i {
            return Err(ParseError::ExpectedType);
        }
        let word = &self.s[start..self.i];
        let abi = match word {
            "slot" => FfiAbiTy::Slot,
            "ptr" => FfiAbiTy::Ptr,
            "i32" => FfiAbiTy::I32,
            "void" => FfiAbiTy::Void,
            other => return Err(ParseError::UnknownAbiType(other.to_string())),
        };
        Ok(FfiTy::Abi(abi))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::rid_hex;

    pub const BUFFER_RID_HEX: &str =
        "867207405fe87fda620c2d7a5485e8e5e274636a898a166fb674448b4391ffc5";
    pub const READER_RID_HEX: &str =
        "9cf620742b025f89429f437163db44cd1c22add25fbc0c7d966c177401f820b4";

    #[test]
    fn parse_i32_roundtrip() {
        // `i32` contains a digit: the type-word lexer must not stop early.
        let parsed = parse_sig("(i32) -> i32").unwrap();
        assert_eq!(parsed.args, vec![FfiTy::Abi(FfiAbiTy::I32)]);
        assert_eq!(parsed.ret, FfiTy::Abi(FfiAbiTy::I32));
        let with_method = parse_sig("(slot) -> slot; read8(slot) -> i32").unwrap();
        assert_eq!(with_method.methods[0].ret, FfiTy::Abi(FfiAbiTy::I32));
    }

    #[test]
    fn parse_buffer_import_sig() {
        let sig = format!("(slot) -> P<{BUFFER_RID_HEX}>; read8(slot) -> slot; size(slot) -> slot");
        let parsed = parse_sig(&sig).unwrap();
        assert_eq!(parsed.args.len(), 1);
        assert!(parsed.ret.is_pit());
        assert_eq!(parsed.methods.len(), 2);
        assert_eq!(parsed.methods[0].name, "read8");
    }

    #[test]
    fn parse_reader_import_sig() {
        let sig = format!("(slot) -> P<{READER_RID_HEX}>; read() -> P<{BUFFER_RID_HEX}>");
        let parsed = parse_sig(&sig).unwrap();
        assert_eq!(parsed.methods.len(), 1);
        assert!(parsed.methods[0].ret.is_pit());
    }
}
