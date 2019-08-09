use std::boxed::Box;
use std::convert::From;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use tokenize::{CodeLocation, Token, TokenKind, TokenStream, TokenizeError};

#[derive(Debug)]
pub enum ParseError<'a> {
    MissingToken,
    ExpectedExprToken(Token<'a>),
    UnexpectedToken {
        token: Token<'a>,
        expected: Option<TokenKind>,
    },
    UnknownToken {
        tokenize_error: TokenizeError<'a>,
    },
    UnexpectedIntegerComparator(Token<'a>),
}

#[derive(Debug)]
pub struct WhileExpr<'a> {
    pub var_idx: u32,
    head_tokens: [Token<'a>; 5], // the loop head has exactly 5 tokens
    end_token: Token<'a>,
    comparator: i32,
    pub body: Box<Expression<'a>>,
}

impl<'a> WhileExpr<'a> {
    pub fn code_location(&self) -> CodeLocation {
        self.head_tokens[0].code_location()
    }
}

#[derive(Debug)]
pub struct LoopExpr<'a> {
    pub var_idx: u32,
    head_tokens: [Token<'a>; 3],
    end_token: Token<'a>,
    pub body: Box<Expression<'a>>,
}

impl<'a> LoopExpr<'a> {
    pub fn code_location(&self) -> CodeLocation {
        self.head_tokens[0].code_location()
    }
}

#[derive(Debug)]
pub struct AssignExpr<'a> {
    pub target_var_idx: u32,
    pub source_var_idx: u32,
    pub modifier: i32,
    tokens: [Token<'a>; 5],
}

impl<'a> AssignExpr<'a> {
    pub fn code_location(&self) -> CodeLocation {
        self.tokens[0].code_location()
    }
}

#[derive(Debug)]
pub struct SequenceExpr<'a> {
    pub body: Vec<Expression<'a>>,
    separator_tokens: Vec<Token<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    While(WhileExpr<'a>),
    Loop(LoopExpr<'a>),
    Assign(AssignExpr<'a>),
    Sequence(SequenceExpr<'a>),
}

type ExpressionResult<'a> = Result<Expression<'a>, ParseError<'a>>;

type PeekableTokenStream<'a> = Peekable<TokenStream<'a>>;

trait UnpackCheckToken<'a> {
    fn unpack_expect_next_token(&mut self, expect: TokenKind) -> Result<Token<'a>, ParseError<'a>>;
}

// sadly this only works smoothly with unit variants
impl<'a> UnpackCheckToken<'a> for PeekableTokenStream<'a> {
    fn unpack_expect_next_token(&mut self, expect: TokenKind) -> Result<Token<'a>, ParseError<'a>> {
        self.next()
            .ok_or(ParseError::MissingToken)?
            .map_err(ParseError::from)
            .and_then(|tok| {
                if tok.kind == expect {
                    Ok(tok)
                } else {
                    Err(ParseError::UnexpectedToken {
                        token: tok.clone(),
                        expected: Some(expect),
                    })
                }
            })
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::MissingToken => {
                write!(f, "Stream ended abruptly. Missing at least one token")
            }
            ParseError::ExpectedExprToken(token) => write!(
                f,
                "Expected the beginning of an expression, found token {} instead",
                token
            ),
            ParseError::UnexpectedToken {
                token,
                ref expected,
            } => {
                if let Some(exp) = expected {
                    write!(f, "Unexpected Token {}, expected token {}", token, exp)
                } else {
                    write!(
                        f,
                        "Unexpected Token {}. Did not expect any tokens here",
                        token
                    )
                }
            }
            ParseError::UnknownToken { tokenize_error } => match tokenize_error {
                TokenizeError::TokenNotRecognized(text_token) => {
                    write!(f, "Unrecognized Token '{}'", text_token)
                }
            },
            ParseError::UnexpectedIntegerComparator(token) => {
                write!(f, "Expected a '0' as comparator in token {}", token)
            }
        }
    }
}

impl<'a> Error for ParseError<'a> {
    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl<'a> From<TokenizeError<'a>> for ParseError<'a> {
    fn from(error: TokenizeError<'a>) -> Self {
        ParseError::UnknownToken {
            tokenize_error: error,
        }
    }
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::While(while_expr) => write!(
                f,
                "<while var_idx={}, comparator={}>(\n {}))",
                while_expr.var_idx, while_expr.comparator, while_expr.body
            ),
            Expression::Loop(loop_expr) => write!(
                f,
                "<loop initial_var_idx={}>(\n {})",
                loop_expr.var_idx, loop_expr.body
            ),
            Expression::Sequence(seq_expr) => {
                write!(f, "<sequence>(")?;
                for i in seq_expr.body.iter() {
                    write!(f, "{}, ", i)?;
                }
                write!(f, ")")
            }
            Expression::Assign(assign_expr) => write!(
                f,
                "<assign target_idx={}, source_idx={}, modifier={}>\n",
                assign_expr.target_var_idx, assign_expr.source_var_idx, assign_expr.modifier
            ),
        }
    }
}

impl<'a> Token<'a> {
    // Second impl
    fn unpack_variable_token(&self) -> Result<u32, ParseError<'a>> {
        if let TokenKind::TVariable(idx) = self.kind {
            Ok(idx)
        } else {
            Err(ParseError::UnexpectedToken {
                token: self.clone(),
                expected: Some(TokenKind::TVariable(0)),
            })
        }
    }

    fn unpack_integer_token(&self) -> Result<i32, ParseError<'a>> {
        if let TokenKind::TInteger(idx) = self.kind {
            Ok(idx)
        } else {
            Err(ParseError::UnexpectedToken {
                token: self.clone(),
                expected: Some(TokenKind::TInteger(0)),
            })
        }
    }
}

impl<'a> Expression<'a> {
    fn compile_assign(token_stream: &mut PeekableTokenStream<'a>) -> ExpressionResult<'a> {
        let target_var_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let target_var_idx = target_var_token.unpack_variable_token()?;

        // the assign token
        let assign_token = token_stream.unpack_expect_next_token(TokenKind::TAssign)?;

        let source_var_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let source_var_idx = source_var_token.unpack_variable_token()?;

        let operator_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let operator_factor: i32 = {
            match operator_token.kind {
                TokenKind::TMinus => Ok(-1),
                TokenKind::TPlus => Ok(1),
                _ => Err(ParseError::UnexpectedToken {
                    token: operator_token.clone(),
                    expected: Some(TokenKind::TMinus),
                }),
            }
        }?;

        let modifier_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let modifier_value = modifier_token.unpack_integer_token()?;

        let modifier = modifier_value as i32 * operator_factor as i32;
        Ok(Expression::Assign(AssignExpr {
            target_var_idx,
            source_var_idx,
            modifier,
            tokens: [
                target_var_token,
                assign_token,
                source_var_token,
                operator_token,
                modifier_token,
            ],
        }))
    }

    fn compile_while(token_stream: &mut PeekableTokenStream<'a>) -> ExpressionResult<'a> {
        // the loop token
        let while_token = token_stream.unpack_expect_next_token(TokenKind::TWhile)?;

        // while variable token
        let while_var_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let while_var_idx = while_var_token.unpack_variable_token()?;

        // unequal token
        let while_unequal_token = token_stream.unpack_expect_next_token(TokenKind::TUnequal)?;

        // while comparator token
        let while_comparator_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let while_comparator_value = while_comparator_token.unpack_integer_token()?;
        if while_comparator_value != 0 {
            return Err(ParseError::UnexpectedIntegerComparator(
                while_comparator_token,
            ));
        }

        // the DO token
        let while_do_token = token_stream.unpack_expect_next_token(TokenKind::TDo)?;
        let while_body = Self::compile_possible_sequence(token_stream)?;

        // the END token
        let while_end_token = token_stream.unpack_expect_next_token(TokenKind::TEnd)?;

        Ok(Expression::While(WhileExpr {
            var_idx: while_var_idx,
            comparator: while_comparator_value,
            head_tokens: [
                while_token,
                while_var_token,
                while_unequal_token,
                while_comparator_token,
                while_do_token,
            ],
            end_token: while_end_token,
            body: Box::new(while_body),
        }))
    }

    fn compile_loop(token_stream: &mut PeekableTokenStream<'a>) -> ExpressionResult<'a> {
        //the loop token
        let loop_token = token_stream.unpack_expect_next_token(TokenKind::TLoop)?;

        // the variable token
        let loop_var_token = token_stream.next().ok_or(ParseError::MissingToken)??;
        let loop_var_idx = loop_var_token.unpack_variable_token()?;

        // the DO token
        let loop_do_token = token_stream.unpack_expect_next_token(TokenKind::TDo)?;

        let loop_body = Self::compile_possible_sequence(token_stream)?;

        // the END token
        let loop_end_token = token_stream.unpack_expect_next_token(TokenKind::TEnd)?;

        Ok(Expression::Loop(LoopExpr {
            var_idx: loop_var_idx,
            head_tokens: [loop_token, loop_var_token, loop_do_token],
            end_token: loop_end_token,
            body: Box::new(loop_body),
        }))
    }

    fn compile_single_expr(token_stream: &mut PeekableTokenStream<'a>) -> ExpressionResult<'a> {
        let first_token: Token = {
            if let Some(res) = token_stream.peek() {
                res.to_owned().map_err(ParseError::from)
            } else {
                Err(ParseError::MissingToken)
            }
        }?;

        match first_token.kind {
            TokenKind::TWhile => Self::compile_while(token_stream),
            TokenKind::TLoop => Self::compile_loop(token_stream),
            TokenKind::TVariable(_) => Self::compile_assign(token_stream),
            _ => Err(ParseError::ExpectedExprToken(first_token)),
        }
    }

    fn is_semicolon_token(possible_token: Option<&Result<Token<'a>, TokenizeError<'a>>>) -> bool {
        if let Some(Ok(token)) = possible_token {
            if let TokenKind::TSemicolon = token.kind {
                return true;
            }
        }
        false
    }

    fn compile_possible_sequence(
        token_stream: &mut PeekableTokenStream<'a>,
    ) -> ExpressionResult<'a> {
        let first_expression = Self::compile_single_expr(token_stream)?;

        if !Self::is_semicolon_token(token_stream.peek()) {
            return Ok(first_expression);
        }

        let mut seq_body = Vec::new();
        let mut seq_separators = Vec::new();

        seq_body.push(first_expression);

        while Self::is_semicolon_token(token_stream.peek()) {
            // read the token, it should be safe to unwrap here
            let sep = token_stream.next().unwrap().unwrap();

            // another expression must follow a semicolon. Could add a
            // non-strict, where and END token or nothing could follow
            let additional_expr = Self::compile_single_expr(token_stream)?;

            seq_separators.push(sep);
            seq_body.push(additional_expr);
        }

        Ok(Expression::Sequence(SequenceExpr {
            body: seq_body,
            separator_tokens: seq_separators,
        }))
    }

    pub fn compile_from_tokens(token_stream: TokenStream<'a>) -> ExpressionResult<'a> {
        let mut peekable_token_stream = token_stream.peekable();
        let ret = Self::compile_possible_sequence(&mut peekable_token_stream);
        if let Some(res) = peekable_token_stream.next() {
            return Err(ParseError::UnexpectedToken {
                token: res?,
                expected: None,
            });
        }
        return ret;
    }
}
