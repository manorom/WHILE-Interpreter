use crate::lexer;
use crate::settings::Settings;
use crate::token::{Token, TokenKind, CodeLocation};
use std::error;
use std::fmt;
use std::iter::Peekable;


type PeekableLexer<'a> = Peekable<lexer::Lexer<'a>>;

#[derive(Debug)]
pub enum ParseError {
    LexerError(lexer::LexerError),
    UnexpectedToken { got: TokenKind, expected: TokenKind },
    UnexpectedTopLevelToken { got: TokenKind },
    UnexpectedConstant { expected: u32, got: u32 },
    UnexpectedEOF,
}

impl<'a> From<lexer::LexerError> for ParseError {
    fn from(error: lexer::LexerError) -> Self {
        ParseError::LexerError(error)
    }
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::LexerError(lexer_error) => write!(f, "{}", lexer_error),
            ParseError::UnexpectedEOF => write!(f, "Unexpected end of file while parsing"),
            ParseError::UnexpectedConstant { expected, got } => {
                write!(f, "Expected constant {expected}, got {got}")
            }
            ParseError::UnexpectedToken { got, expected } => {
                write!(f, "Expected token {expected}, got {got}")
            }
            ParseError::UnexpectedTopLevelToken { got } => write!(
                f,
                "Expected Loop, While or Assignmnet statements, got {got}"
            ),
        }
    }
}

pub struct Assign {
    pub target_var_idx: u32,
    pub source_var_idx: u32,
    pub modifier: i32,
    pub location: CodeLocation
}

pub struct While {
    pub var_idx: u32,
    pub comparator: i32,
    body: Box<Node>,
    pub location: CodeLocation
}

impl While {
    pub fn body_ref(&self) -> &Node {
        self.body.as_ref()
    }
}

pub struct Loop {
    pub var_idx: u32,
    body: Box<Node>,
    pub location: CodeLocation
}

impl Loop {
    pub fn body_ref(&self) -> &Node {
        self.body.as_ref()
    }
}

pub type Sequence = Vec<Node>;

pub enum Node {
    Sequence(Sequence),
    Assign(Assign),
    While(While),
    Loop(Loop),
}

impl Node {
    pub fn location(&self) -> CodeLocation {
        match self {
            Node::Assign(a) => a.location,
            Node::While(w) => w.location,
            Node::Loop(l) => l.location,
            Node::Sequence(s) => {
                s[0].location()
            }
        }
    }
    pub fn print(&self) {
        print!("(");
        match self {
            Node::Sequence(vec) => {
                print!("(seq ");
                for e in vec {
                    e.print();
                }
                print!(")")
            }
            Node::Assign(assign) => {
                let op = if assign.modifier > 0{
                    "+"
                } else {
                    "-"
                };
                let s = assign.source_var_idx;
                let t = assign.target_var_idx;
                print!("(assign {op} (var {t}) (var {s}))");
            },
            Node::While(w) => {
                print!("(while ");
                w.body.print();
                print!(")");
            }
            Node::Loop(l) => {
                print!("(loop ");
                l.body.print();
                print!(")");
            }
        }
        print!(")");
    }
}

struct Parser<'a> {
    token_stream: PeekableLexer<'a>,
    settings: Settings,
}

impl<'a> Parser<'a> {
    fn expect_token(&mut self) -> Result<Token<'a>, ParseError> {
        Ok(self
            .token_stream
            .next()
            .ok_or(ParseError::UnexpectedEOF)??)
    }
    fn expect_token_kind(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        match self.token_stream.next() {
            None => Err(ParseError::UnexpectedEOF),
            Some(Err(e)) => Err(ParseError::LexerError(e.clone())),
            Some(Ok(t)) if t.kind == kind => Ok(t),
            Some(Ok(t)) => Err(ParseError::UnexpectedToken {
                expected: kind,
                got: t.kind,
            }),
        }
    }

    fn expect_and_unpack_variable(&mut self) -> Result<u32, ParseError> {
        match self.expect_token()?.kind {
            TokenKind::TVariable(i) => Ok(i),
            t => {
                return Err(ParseError::UnexpectedToken {
                    got: t,
                    expected: TokenKind::TVariable(0),
                })
            }
        }
    }

    fn expect_and_unpack_constant(&mut self) -> Result<u32, ParseError> {
        match self.expect_token()?.kind {
            TokenKind::TInteger(i) => Ok(i),
            t => {
                return Err(ParseError::UnexpectedToken {
                    got: t,
                    expected: TokenKind::TVariable(0),
                })
            }
        }
    }

    fn parse_assign(&mut self, first_token: Token<'a>) -> Result<Node, ParseError> {
        let location = first_token.code_location.clone();
        let target_var_idx = match first_token.kind {
            TokenKind::TVariable(i) => i,
            t => {
                return Err(ParseError::UnexpectedToken {
                    got: t,
                    expected: TokenKind::TVariable(0),
                })
            }
        };

        // the assign token
        self.expect_token_kind(TokenKind::TAssign)?;

        let source_var_idx = self.expect_and_unpack_variable()?;

        // First operator token i.e. + or -
        let operator_token = self.expect_token()?;
        let operator_factor: i32 = {
            match operator_token.kind {
                TokenKind::TMinus => Ok(-1),
                TokenKind::TPlus => Ok(1),
                _ => Err(ParseError::UnexpectedToken {
                    got: operator_token.kind,
                    expected: TokenKind::TMinus,
                }),
            }
        }?;

        // some versions of while specification would allow two operator tokens
        // (if the assignment constant is allowd to be signed)
        let mut second_operator_factor = 1;
        if self
            .token_stream
            .peek()
            .map(|r| r.as_ref().map(|t| t.is_operator_token()).unwrap_or(false))
            == Some(true)
        {
            let second_operator_token = self.expect_token()?;
            second_operator_factor = match second_operator_token.kind {
                TokenKind::TPlus => Ok(1),
                TokenKind::TMinus => Ok(-1),
                _ => Err(ParseError::UnexpectedToken {
                    got: second_operator_token.kind,
                    expected: TokenKind::TPlus,
                }),
            }?;
        }

        let modifier_token = self.expect_token()?;
        let modifier_value = match modifier_token.kind {
            TokenKind::TInteger(i) => i,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    got: modifier_token.kind,
                    expected: TokenKind::TInteger(0),
                })
            }
        };

        if !self.settings.allow_arbitrary_constants_assign
            && !(modifier_value == 1 || modifier_value == 0)
        {
            return Err(ParseError::UnexpectedConstant {
                expected: 1,
                got: modifier_value,
            });
        }

        let modifier =
            modifier_value as i32 * operator_factor as i32 * second_operator_factor as i32;

        Ok(Node::Assign(Assign {
            target_var_idx,
            source_var_idx,
            modifier,
            location
        }))
    }

    fn parse_loop(&mut self, first_token: &Token) -> Result<Node, ParseError> {
        let location = first_token.code_location;
        // "TLoop" is already consumed

        // the variable token
        self.expect_token_kind(TokenKind::TLoop)?;
        let loop_var_idx = self.expect_and_unpack_variable()?;

        // the DO token
        self.expect_token_kind(TokenKind::TDo)?;

        let body = self.parse()?;

        // the END token
        self.expect_token_kind(TokenKind::TEnd)?;

        Ok(Node::Loop(Loop {
            var_idx: loop_var_idx,
            body: Box::new(body),
            location
        }))
    }

    fn parse_while(&mut self, first_token: &Token) -> Result<Node, ParseError> {
        // the loop token
        let location = first_token.code_location.clone();

        // while variable token
        let var_idx = self.expect_and_unpack_variable()?;

        // unequal token
        self.expect_token_kind(TokenKind::TUnequal)?;

        // while comparator token
        let comparator_value = self.expect_and_unpack_constant()? as i32;
        if comparator_value != 0 {
            return Err(ParseError::UnexpectedConstant {
                expected: 0,
                got: comparator_value as u32,
            });
        }

        // the DO token
        self.expect_token_kind(TokenKind::TDo)?;

        let body = self.parse()?;

        // the END token
        self.expect_token_kind(TokenKind::TEnd)?;

        Ok(Node::While(While {
            var_idx,
            comparator: comparator_value,
            body: Box::new(body),
            location
        }))
    }

    fn parse_stmt(&mut self) -> Result<Node, ParseError> {
        let first_token = self
            .token_stream
            .next()
            .ok_or(ParseError::UnexpectedEOF)??;
        match first_token.kind {
            TokenKind::TWhile => self.parse_while(&first_token),
            TokenKind::TLoop => self.parse_loop(&first_token),
            TokenKind::TVariable(_) => self.parse_assign(first_token),
            other => Err(ParseError::UnexpectedTopLevelToken { got: other }),
        }
    }
    fn peek_semicolon(&mut self) -> bool {
        if let Some(Ok(t)) = self.token_stream.peek() {
            t.kind == TokenKind::TSemicolon
        } else {
            false
        }
    }
    fn parse_remaining_sequence(&mut self, first_stmt: Node) -> Result<Node, ParseError> {
        let mut seq = vec![first_stmt];
        while self.peek_semicolon() {
            self.token_stream.next();
            let next_stmt = self.parse_stmt()?;
            seq.push(next_stmt);
        }
        Ok(Node::Sequence(seq))
    }
    fn parse(&mut self) -> Result<Node, ParseError> {
        let first_stmt = self.parse_stmt()?;
        match self.token_stream.peek() {
            None => return Ok(first_stmt),
            Some(Err(e)) => Err(ParseError::LexerError(e.clone())),
            Some(Ok(t)) if t.is_semicolon() => return self.parse_remaining_sequence(first_stmt),
            Some(Ok(t)) => Err(ParseError::UnexpectedTopLevelToken { got: t.kind }),
        }
    }

    pub fn parse_program(&mut self) -> Result<Node, ParseError> {
        let program = self.parse()?;

        // check if we have any tokens left
        if let Some(token) = self.token_stream.next() {
            return Err(ParseError::UnexpectedTopLevelToken { got: token?.kind });
        }

        Ok(program)
    }
}

pub fn parse_program<'a>(lexer: lexer::Lexer<'a>, settings: Settings) -> Result<Node, ParseError> {
    let mut parser = Parser {
        token_stream: lexer.peekable(),
        settings
    };

    parser.parse_program()
}