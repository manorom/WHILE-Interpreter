use crate::settings::Settings;
use crate::token::{CodeLocation, Token, TokenKind};
use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub enum LexerError {
    TokenUnrecognized {
        code_location: CodeLocation,
        fragment: String,
    },
}

impl LexerError {
    fn set_code_location(&mut self, new_code_location: CodeLocation) {
        match self {
            LexerError::TokenUnrecognized {
                ref mut code_location,
                ..
            } => {
                *code_location = new_code_location;
            }
        }
    }

    fn token_unrecognized(fragment: String) -> LexerError {
        LexerError::TokenUnrecognized {
            code_location: CodeLocation::default(),
            fragment,
        }
    }
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::TokenUnrecognized {
                code_location,
                fragment,
            } => write!(
                f,
                "Could not recognize fragment \"{}\" as token in line {}",
                fragment, code_location
            ),
        }
    }
}

#[derive(Clone, Debug)]
struct CodeLocationIterator<'a> {
    iter: std::str::Chars<'a>,
    peeking_iter: std::str::Chars<'a>,
    peeked_value: Option<char>,
    current_location: CodeLocation,
}

impl<'a> CodeLocationIterator<'a> {
    fn new(input_str: &'a str) -> CodeLocationIterator<'a> {
        CodeLocationIterator {
            iter: input_str.chars(),
            peeking_iter: input_str.chars(),
            peeked_value: None,
            current_location: CodeLocation { line: 1, col: 1 },
        }
    }

    fn as_str(&self) -> &'a str {
        self.iter.as_str()
    }

    fn peek(&mut self) -> Option<char> {
        if self.peeked_value.is_none() {
            self.peeked_value = self.peeking_iter.next();
        }
        self.peeked_value
    }
}

impl<'a> Iterator for CodeLocationIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let next = self.iter.next();
        match next {
            Some('\n') => {
                self.current_location.line += 1;
                self.current_location.col = 1;
            }
            _ => {
                self.current_location.col += 1;
            }
        }

        if self.peeked_value.is_some() {
            self.peeked_value = None;
        } else {
            self.peeking_iter.next();
        }

        next
    }
}

pub struct Lexer<'a> {
    iter: CodeLocationIterator<'a>,
    settings: &'a Settings,
}

impl<'a> Lexer<'a> {
    pub fn new(input_str: &'a str, settings: &'a Settings) -> Lexer<'a> {
        Lexer {
            iter: CodeLocationIterator::new(input_str),
            settings,
        }
    }

    fn try_lex_keyword(&mut self, keyword: &str, kind: TokenKind) -> Result<TokenKind, LexerError> {
        let s = self.iter.as_str();

        if let Some(subslice) = s.get(..keyword.len()) {
            if subslice == keyword {
                // move the iterator to the end of the keyword
                self.iter.nth(keyword.len() - 1);
                // if we expect a delimiter (whitespace, newline or semicolon) after a keyword,
                // check for that
                if self.settings.delimiter_after_keyword {
                    match self.iter.peek() {
                        Some(c) if c.is_whitespace() => (),
                        Some('\n') | Some(';') => (),
                        None => (), // EOF is also acceptable
                        _ => return Err(self.lex_unknown_fragment(s, keyword.len())),
                    }
                }
                return Ok(kind);
            }
        }
        Err(self.lex_unknown_fragment(s, 0))
    }

    fn try_lex_unsigned_int(&mut self) -> Result<TokenKind, LexerError> {
        let s = self.iter.as_str();
        let mut num_of_digits = 0;

        while self.iter.peek().map_or(false, |c| c.is_digit(10)) {
            self.iter.next();
            num_of_digits += 1;
        }

        let unsigned_int = s[..num_of_digits].parse::<u32>().unwrap();
        return Ok(TokenKind::TInteger(unsigned_int));
    }

    // TODO: Error handling for the case that num_of_lexed_chars == 0
    fn try_lex_variable(&mut self) -> Result<TokenKind, LexerError> {
        let s = self.iter.as_str();
        let mut num_of_lexed_chars = 1;
        // we already know that the first char is an 'x'
        self.iter.next();

        while self.iter.peek().map_or(false, |c| c.is_digit(10)) {
            self.iter.next();
            num_of_lexed_chars += 1;
        }

        if num_of_lexed_chars == 1 {
            return Err(self.lex_unknown_fragment(s, 1));
        }

        let var_index = s[1..num_of_lexed_chars].parse::<u32>().unwrap();
        return Ok(TokenKind::TVariable(var_index));
    }

    fn is_delimiter_for_unknown_fragment(&self, character: char) -> bool {
        match character {
            r if r.is_digit(10) => return true,
            r if r.is_whitespace() => return true,
            ';' | '\n' | '!' | ':' | '+' | '-' => return true,
            _ => return false,
        }
    }

    fn lex_unknown_fragment(&mut self, lex_str: &'a str, initial_lex_len: usize) -> LexerError {
        let mut num_of_lexed_chars = initial_lex_len;
        while self
            .iter
            .peek()
            .map_or(false, |c| !self.is_delimiter_for_unknown_fragment(c))
        {
            self.iter.next();
            num_of_lexed_chars += 1;
        }

        return LexerError::token_unrecognized(lex_str[..num_of_lexed_chars].to_owned());
    }

    fn try_lex_additional_char(
        &mut self,
        additional_char: char,
        target_token_kind: TokenKind,
        input_str: &'a str,
    ) -> Result<TokenKind, LexerError> {
        self.iter.next();
        if Some(additional_char) == self.iter.peek() {
            self.iter.next();
            return Ok(target_token_kind);
        }
        return Err(self.lex_unknown_fragment(input_str, 1));
    }

    fn lex(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        while let Some(character) = self.iter.peek() {
            let token_code_location = self.iter.current_location.clone();
            let s = self.iter.as_str();

            let possible_next_token_kind: Option<Result<TokenKind, LexerError>> = match character {
                r if r.is_whitespace() => {
                    self.iter.next();
                    None
                }
                '\t' | '\n' => {
                    self.iter.next();
                    None
                }
                '#' => {
                    while self.iter.peek().map_or(false, |c| c != '\n') {
                        self.iter.next();
                    }
                    None
                }
                '+' => {
                    self.iter.next();
                    Some(Ok(TokenKind::TPlus))
                }
                '-' => {
                    self.iter.next();
                    Some(Ok(TokenKind::TMinus))
                }
                ';' => {
                    self.iter.next();
                    Some(Ok(TokenKind::TSemicolon))
                }
                ':' => Some(self.try_lex_additional_char('=', TokenKind::TAssign, s)),
                '!' => Some(self.try_lex_additional_char('=', TokenKind::TUnequal, s)),
                'x' => Some(self.try_lex_variable()),
                'D' => Some(self.try_lex_keyword("DO", TokenKind::TDo)),
                'W' => Some(self.try_lex_keyword("WHILE", TokenKind::TWhile)),
                'L' => Some(self.try_lex_keyword("LOOP", TokenKind::TLoop)),
                'E' => Some(self.try_lex_keyword("END", TokenKind::TEnd)),
                _ if character.is_digit(10) => Some(self.try_lex_unsigned_int()),
                _ => Some(Err(self.lex_unknown_fragment(s, 0))),
            };

            if let Some(mut next_token_kind) = possible_next_token_kind {
                let token_len = self.iter.current_location.col - token_code_location.col;

                // set the code location in case of an error
                if let Err(ref mut lexer_error) = next_token_kind {
                    lexer_error.set_code_location(token_code_location);
                }

                return Some(next_token_kind.map(|kind| Token {
                    kind: kind,
                    source_text: &s[..token_len],
                    code_location: token_code_location,
                }));
            }
        }
        None
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        self.lex()
    }
}
