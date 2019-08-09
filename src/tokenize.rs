use std::error::Error;
use std::fmt;
use std::str::Chars;

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Lexeme<'a> {
    text: &'a str,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
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
    TInteger(i32),
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    source_text: Lexeme<'a>,
}

#[derive(Debug, Clone)]
pub enum TokenizeError<'a> {
    TokenNotRecognized(Lexeme<'a>),
}

#[derive(Clone)]
struct LexemeStream<'a> {
    it: Chars<'a>,
    cur_line_num: usize,
    cur_col_num: usize,
}

#[derive(Clone)]
pub struct TokenStream<'a> {
    it: LexemeStream<'a>,
}

impl<'a> Token<'a> {
    pub fn code_location(&self) -> CodeLocation {
        CodeLocation {
            line: self.source_text.line,
            col: self.source_text.col,
        }
    }
}

impl<'a> fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token_text = match self {
            TokenKind::TWhile => "WHILE".to_string(),
            TokenKind::TDo => "DO".to_string(),
            TokenKind::TLoop => "LOOP".to_string(),
            TokenKind::TEnd => "END".to_string(),
            TokenKind::TPlus => "+".to_string(),
            TokenKind::TMinus => "-".to_string(),
            TokenKind::TSemicolon => ";".to_string(),
            TokenKind::TUnequal => "!=".to_string(),
            TokenKind::TAssign => ":=".to_string(),
            TokenKind::TVariable(idx) => format!("x{}", idx),
            TokenKind::TInteger(i) => i.to_string(),
        };
        write!(f, "{}", token_text)
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} at line {}:{}",
            self.kind, self.source_text.line, self.source_text.col
        )
    }
}

impl<'a> fmt::Display for Lexeme<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "('{}' at {}:{})", self.text, self.line, self.col)
    }
}

impl<'a> fmt::Display for TokenizeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenizeError::TokenNotRecognized(text_token) => write!(
                f,
                "Unrecognized Token '{}' in line {}:{}",
                text_token.text, text_token.line, text_token.col
            ),
        }
    }
}

impl<'a> Error for TokenizeError<'a> {
    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl<'a> Iterator for LexemeStream<'a> {
    type Item = Lexeme<'a>;

    fn next(&mut self) -> Option<Lexeme<'a>> {
        while let Some(ch) = self.it.clone().next() {
            // we need .as_str and thus cannot use peekable
            let s = self.it.as_str();
            match ch {
                ' ' | '\t' => {
                    self.it.next();
                    self.cur_col_num += 1;
                }
                '\n' => {
                    self.it.next();
                    self.cur_col_num = 0;
                    self.cur_line_num += 1;
                }
                '+' | '-' | ';' => {
                    let ret = Some(Lexeme {
                        text: &s[..1],
                        line: self.cur_line_num,
                        col: self.cur_col_num,
                    });
                    self.it.next();
                    self.cur_col_num += 1;
                    return ret;
                }
                '!' | ':' => {
                    self.it.next();
                    let mut token_size = 1;

                    if self.it.clone().next() == Some('=') {
                        self.it.next();
                        token_size += 1;
                    }

                    let ret = Some(Lexeme {
                        text: &s[..token_size],
                        line: self.cur_line_num,
                        col: self.cur_col_num,
                    });
                    self.cur_col_num += token_size;
                    return ret;
                }
                _ => {
                    let mut local_it = self.it.clone();
                    let mut token_size = 0;
                    while local_it.next().map_or(false, |ch| match ch {
                        ' ' | '\t' | '\n' | '!' | ':' | ';' | '+' | '-' => false,
                        _ => true,
                    }) {
                        self.it.next();
                        token_size += 1;
                    }

                    let ret = Some(Lexeme {
                        text: &s[..token_size],
                        line: self.cur_line_num,
                        col: self.cur_col_num,
                    });
                    self.cur_col_num += token_size;
                    return ret;
                }
            }
        }
        None
    }
}

impl<'a> LexemeStream<'a> {
    fn from_str(text: &'a str) -> LexemeStream {
        LexemeStream {
            it: text.chars(),
            cur_line_num: 1,
            cur_col_num: 1,
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token<'a>, TokenizeError<'a>>;

    fn next(&mut self) -> Option<Result<Token<'a>, TokenizeError<'a>>> {
        self.it.next().and_then(|text_token| {
            let kind_res: Result<TokenKind, TokenizeError<'a>> = match text_token.text {
                "WHILE" => Ok(TokenKind::TWhile),
                "DO" => Ok(TokenKind::TDo),
                "LOOP" => Ok(TokenKind::TLoop),
                "END" => Ok(TokenKind::TEnd),
                "+" => Ok(TokenKind::TPlus),
                "-" => Ok(TokenKind::TMinus),
                ";" => Ok(TokenKind::TSemicolon),
                "!=" => Ok(TokenKind::TUnequal),
                ":=" => Ok(TokenKind::TAssign),
                text => {
                    let mut ret = Err(TokenizeError::TokenNotRecognized(text_token.clone()));
                    if let Ok(i) = text.parse::<i32>() {
                        ret = Ok(TokenKind::TInteger(i))
                    } else {
                        let mut text_iter = text.chars();
                        if text_iter.next() == Some('x') {
                            if let Ok(i) = text_iter.as_str().parse::<u32>() {
                                ret = Ok(TokenKind::TVariable(i))
                            }
                        }
                    }
                    ret
                }
            };

            Some(kind_res.and_then(|kind| {
                Ok(Token {
                    kind,
                    source_text: text_token,
                })
            }))
        })
    }
}

impl<'a> TokenStream<'a> {
    pub fn from_str(text: &'a str) -> TokenStream {
        TokenStream {
            it: LexemeStream::from_str(text),
        }
    }
}
