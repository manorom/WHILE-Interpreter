use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::convert::From;
use std::boxed::Box;
use tokenize::{Token, TokenKind, TokenStream, TokenizeError};

#[derive(Debug)]
pub enum CompileError<'a> {
    MissingToken,
    ExpectedExprToken(Token<'a>),
    UnexpectedToken { token: Token<'a>,
                      expected: TokenKind },
    UnknownToken { tokenize_error: TokenizeError<'a> }
}

impl<'a> fmt::Display for CompileError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::MissingToken => {
                write!(f, "Stream ended abruptly. Missing at least one token")
            },
            CompileError::ExpectedExprToken(token) => {
                write!(f, "Expected the beginning of an expression, found token {} instead", token)
            },
            CompileError::UnexpectedToken{ token, expected } => {
                write!(f, "Unexpected Token {}, expected token {}", token, expected)
            },
            CompileError::UnknownToken { tokenize_error } => {
                match tokenize_error {
                    TokenizeError::TokenNotRecognized(text_token) =>
                        write!(f, "Unrecognized Token '{}'", text_token),
                }
            }
        }
    }
}

impl<'a> Error for CompileError<'a> {
    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl<'a> From<TokenizeError<'a>> for CompileError<'a> {
    fn from(error: TokenizeError<'a>) -> Self {
        CompileError::UnknownToken{tokenize_error: error}
    }
}

impl<'a> Token<'a> { // Second impl
    fn unpack_variable_token(&self) -> Result<u32, CompileError<'a>> {
        if let TokenKind::TVariable(idx) = self.kind {
            Ok(idx)
        } else {
            Err(CompileError::UnexpectedToken{
                token: self.clone(),
                expected: TokenKind::TVariable(0)
            })
        }
    }

    fn unpack_integer_token(&self) -> Result<i32, CompileError<'a>> {
        if let TokenKind::TInteger(idx) = self.kind {
            Ok(idx)
        } else {
            Err(CompileError::UnexpectedToken{
                token: self.clone(),
                expected: TokenKind::TInteger(0)
            })
        }
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    WhileExpr{ var_idx: u32,
               comparator: i32,
               head_tokens: [Token<'a>; 5], // the loop head has exactly 5 tokens
               end_token: Token<'a>,
               body: Box<Expression<'a>> },
    LoopExpr{ var_idx: u32,
              head_tokens: [Token<'a>; 3],
              end_token: Token<'a>,
              body: Box<Expression<'a>> },
    AssignExpr{ target_var_idx: u32,
                source_var_idx: u32,
                modifier: i64,
                tokens: [Token<'a>; 5],},
    SequenceExpr{ body: Vec<Expression<'a>>,
                  seperator_tokens: Vec<Token<'a>> },
}

type ExpressionResult<'a> = Result<Expression<'a>, CompileError<'a>>;

type PeekableTokenStream<'a> = Peekable<TokenStream<'a>>;

impl<'a> Expression<'a> {

    fn compile_assign(token_stream: &mut PeekableTokenStream<'a>)
            -> ExpressionResult<'a> {
        // we already have the first token
        let target_var_token = token_stream.next()
                .ok_or(CompileError::MissingToken)??;
        let target_var_idx = target_var_token.unpack_variable_token()?;

        // the loop token
        let assign_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TAssign = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token: tok.clone(),
                        expected: TokenKind::TWhile
                    })
                }
            })?;
        let source_var_token = token_stream.next().ok_or(CompileError::MissingToken)??;
        let source_var_idx = source_var_token.unpack_variable_token()?;

        let operator_token = token_stream.next().ok_or(CompileError::MissingToken)??;
        let operator_factor: i32 = {
            match operator_token.kind {
                TokenKind::TMinus => Ok(-1),
                TokenKind::TPlus  => Ok(1),
                _ => Err(CompileError::UnexpectedToken{ token: operator_token.clone(),
                                                        expected: TokenKind::TMinus })
            }
        }?;

        let modifier_token = token_stream.next().ok_or(CompileError::MissingToken)??;
        let modifier_value = modifier_token.unpack_integer_token()?;

        let modifier = modifier_value as i64 * operator_factor as i64;
        Ok(Expression::AssignExpr{ target_var_idx,
                                   source_var_idx,
                                   modifier,
                                   tokens: [target_var_token, assign_token,
                                     source_var_token, operator_token,
                                     modifier_token] })
    }

    fn compile_while(token_stream: &mut PeekableTokenStream<'a>)
            -> ExpressionResult<'a> {
        // the loop token
        let while_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TWhile = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token: tok.clone(),
                        expected: TokenKind::TWhile
                    })
                }
            })?;

        // while variable token
        let while_var_token = token_stream.next().ok_or(CompileError::MissingToken)??;
        let while_var_idx = while_var_token.unpack_variable_token()?;

        // unequal token
        let while_unequal_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TUnequal = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token: tok.clone(),
                        expected: TokenKind::TUnequal
                    })
                }
            })?;

        // while comparator token
        let while_comparator_token = token_stream.next()
            .ok_or(CompileError::MissingToken)??;
        let while_comparator_value = while_comparator_token
            .unpack_integer_token()?;

        // the DO token
        let while_do_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TDo = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token:tok.clone(),
                        expected: TokenKind::TDo
                    })
                }
            })?;

        let while_body = Self::compile_possible_sequence(token_stream)?;

        // the END token
        let while_end_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TEnd = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token:tok.clone(),
                        expected: TokenKind::TEnd
                    })
                }
            })?;

        Ok(Expression::WhileExpr{
            var_idx: while_var_idx,
            comparator: while_comparator_value,
            head_tokens: [while_token, while_var_token,
                          while_unequal_token,
                          while_comparator_token, while_do_token],
            end_token: while_end_token,
            body: Box::new(while_body)
        })
    }

    fn compile_loop(token_stream: &mut PeekableTokenStream<'a>)
            -> ExpressionResult<'a> {
        //the loop token
        let loop_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TLoop = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token: tok.clone(),
                        expected: TokenKind::TLoop
                    })
                }
            })?;

        // the variable token
        let loop_var_token = token_stream.next()
            .ok_or(CompileError::MissingToken)??;
        let loop_var_idx = loop_var_token.unpack_variable_token()?;

        // the DO token
        let loop_do_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TDo = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token:tok.clone(),
                        expected: TokenKind::TDo
                    })
                }
            })?;

        let loop_body =  Self::compile_possible_sequence(token_stream)?;

        // the END token
        let loop_end_token = token_stream.next()
            .ok_or(CompileError::MissingToken)?
            .map_err(CompileError::from)
            .and_then(|tok| {
                if let TokenKind::TEnd = tok.kind {
                    Ok(tok)
                } else {
                    Err(CompileError::UnexpectedToken{
                        token:tok.clone(),
                        expected: TokenKind::TEnd
                    })
                }
            })?;

        Ok(Expression::LoopExpr{
            var_idx: loop_var_idx,
            head_tokens: [loop_token, loop_var_token, loop_do_token],
            end_token: loop_end_token,
            body: Box::new(loop_body)
        })
    }

    fn compile_single_expr(token_stream: &mut PeekableTokenStream<'a>)
            -> ExpressionResult<'a> {

        let first_token: Token = {
            if let Some(res) = token_stream.peek() {
                res.to_owned()
                   .map_err(CompileError::from)
            } else {
                Err(CompileError::MissingToken)
            }
        }?;

        match first_token.kind {
            TokenKind::TWhile => {
                Self::compile_while(token_stream)
            },
            TokenKind::TLoop => {
                Self::compile_loop(token_stream)
            },
            TokenKind::TVariable(_) => {
                Self::compile_assign(token_stream)
            },
            _ => {
                Err(CompileError::ExpectedExprToken(first_token))
            }
        }
    }

    fn is_semicolon_token(possible_token: Option<&Result<Token<'a>,
                                                 TokenizeError<'a>>>) -> bool {
        if let Some(Ok(token)) = possible_token {
            if let TokenKind::TSemicolon = token.kind {
                return true;
            }
        }
        false
    }

    fn compile_possible_sequence(token_stream: &mut PeekableTokenStream<'a>)
            -> ExpressionResult<'a> {
        let first_expression = Self::compile_single_expr(token_stream)?;

        if !Self::is_semicolon_token(token_stream.peek()) {
            return Ok(first_expression);
        }

        let mut seq_body = Vec::new();
        let mut seq_seperators = Vec::new();

        seq_body.push(first_expression);

        while Self::is_semicolon_token(token_stream.peek()) {
            // read the token, it should be safe to unwrap here
            let sep = token_stream.next().unwrap().unwrap();

            // another expression must follow a semicolon. Could add a
            // non-strict, where and END token or nothing could follow
            let additional_expr = Self::compile_single_expr(token_stream)?;

            seq_seperators.push(sep);
            seq_body.push(additional_expr);
        }

        Ok(Expression::SequenceExpr{ body: seq_body,
                                     seperator_tokens: seq_seperators })
    }

    pub fn compile_from_tokens(token_stream: TokenStream<'a>)
            -> ExpressionResult<'a> {
        Self::compile_possible_sequence(&mut token_stream.peekable())
    }
}
