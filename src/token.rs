use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    pub line: usize,
    pub col: usize,
}

impl Default for CodeLocation {
    fn default() -> CodeLocation {
        CodeLocation {
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for CodeLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    TWhile,
    TDo,
    TLoop,
    TEnd,
    TPlus,
    TMinus,
    TSemicolon,
    TUnequal,
    TAssign,
    TVariable(u32),
    TInteger(u32),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self{
            TokenKind::TWhile => {
                write!(f, "TWhile")
            },
            TokenKind::TDo => {
                write!(f, "TDo")
            },
            TokenKind::TLoop => {
                 write!(f, "TLoop")
            },
            TokenKind::TEnd => {
                 write!(f, "TEnd")
            },
            TokenKind::TPlus => {
                 write!(f, "TPlus")
            },
            TokenKind::TMinus => {
                 write!(f, "TMinus")
            },
            TokenKind::TSemicolon => {
                 write!(f, "TSemicolon")
            },
            TokenKind::TUnequal => {
                 write!(f, "TUnequal")
            },
            TokenKind::TAssign => {
                 write!(f, "TAssign")
            },
            TokenKind::TVariable(_) => {
                write!(f, "TVariable")
            },
            TokenKind::TInteger(_) => {
                write!(f, "TInteger")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub source_text: &'a str,
    pub code_location: CodeLocation,
}

impl<'a> Token<'a> {
    pub fn code_location(&self) -> CodeLocation {
        self.code_location
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::TVariable(idx) => {
                write!(f, "TVariable[{}, idx:{}]", self.code_location, idx)
            },
            TokenKind::TInteger(val) => {
                write!(f, "TInteger[{}, val:{}]", self.code_location, val)
            },
            k => {
                write!(f, "{}[{}]", k, self.code_location)
            }
        }
    }
}