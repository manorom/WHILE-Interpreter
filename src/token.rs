use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    pub line: usize,
    pub col: usize,
}

impl Default for CodeLocation {
    fn default() -> CodeLocation {
        CodeLocation { line: 0, col: 0 }
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
        match self {
            TokenKind::TWhile => write!(f, "WHILE"),
            TokenKind::TDo => write!(f, "DO"),
            TokenKind::TLoop => write!(f, "LOOP"),
            TokenKind::TEnd => write!(f, "END"),
            TokenKind::TPlus => write!(f, "+"),
            TokenKind::TMinus => write!(f, "-"),
            TokenKind::TSemicolon => write!(f, ";"),
            TokenKind::TUnequal => write!(f, "!="),
            TokenKind::TAssign => write!(f, ":="),
            TokenKind::TVariable(_) => write!(f, "Variable"),
            TokenKind::TInteger(_) => write!(f, "Integer"),
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
    pub fn is_semicolon(&self) -> bool {
        self.kind == TokenKind::TSemicolon
    }
    pub fn is_operator_token(&self) -> bool {
        match self.kind {
            TokenKind::TPlus | TokenKind::TMinus => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::TVariable(idx) => {
                write!(f, "[Variable 'x{}' at {}]", idx, self.code_location)
            }
            TokenKind::TInteger(val) => {
                write!(f, "[Integer with value {} at {}]", val, self.code_location)
            }
            k => write!(f, "['{}' at {}]", k, self.code_location),
        }
    }
}
