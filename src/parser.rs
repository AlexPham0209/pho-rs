use crate::{
    expr::{self, And, Expr, Logical, LogicalOp, Or, Statement, Variable},
    lexer::{Token, TokenType},
};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    curr: usize,
}

pub enum Error {
    ExpectedExpression,
    UnexpectedToken(TokenType),
    TokenMismatch {
        expected: TokenType,
        actual: TokenType,
        line: usize,
        col: usize,
    },
    EOS,
    DeclarationError,
}

impl<'a> Parser<'a> {
    fn from_tokens(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser { tokens, curr: 0 }
    }
    fn parse(&mut self) {
        while !self.is_at_end() {
            self.declaration();
        }
    }

    fn declaration(&mut self) -> Result<Statement, Error> {
        let token = match self.current() {
            // Some(TokenType::Class) => Ok(self.class()),
            // Some(TokenType::Function) => Ok(self.function()),
            Some(_) => self.statement(),
            None => Err(Error::DeclarationError),
        };

        token
    }

    // fn class(&mut self) -> Result<Statement, Error> {
    //     self.advance();
    //     let identifier_token = self.advance().expect("Expected identifier");
    //     let identifier = match &identifier_token.token_type {
    //         TokenType::Identifier(val) => val,
    //         _ => panic!("Expected Identifier")
    //     };

    //     self.consume(TokenType::OpenCurlyBrace, "Expected { after identifier");
    //     let variables = Vec::<Variable>::new();
    //     while let Some(token) = self.current() {
    //         let tokens = match token {
    //             TokenType::Variable => {
    //                 let variable = self.variable();

    //             },
    //             TokenType::Function => Ok(self.function()),
    //             _ => return Err(Error::DeclarationError)
    //         };
    //     }

    //     self.consume(TokenType::CloseCurlyBrace, "Expected } after expression");
    // }

    fn statement(&mut self) -> Result<Statement, Error> {
        match self.current() {
            Some(TokenType::Variable) => self.variable(),
            Some(token) => Err(Error::UnexpectedToken(token)),
            None => Err(Error::EOS),
        }
    }

    fn variable(&mut self) -> Result<Statement, Error> {
        let identifier = self.consume(TokenType::Identifier)?;
        let lexeme = identifier.lexeme.clone();
        self.consume(TokenType::Equal)?;
        let expression = self.expression()?;

        Ok(Statement::Variable(Variable {
            identifier: lexeme,
            value: Box::new(expression),
        }))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        let or = self.or()?;
        Ok(or)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let left = self.and()?;

        if !self.check(TokenType::Or) {
            let right: Expr = self.or()?;
            return Ok(Expr::Logical(Logical {
                op: LogicalOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let left = self.equality()?;

        if !self.check(TokenType::And) {
            let right: Expr = self.equality()?;
            return Ok(Expr::Logical(Logical {
                op: LogicalOp::And,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let left = self.comparison()?;

        while self.check(TokenType::Equal) || self.check(TokenType::NotEqual) {
            let op = match self.prev().unwrap().token_type {
                TokenType::Equal => LogicalOp::Equal,
                TokenType::NotEqual => LogicalOp::NotEqual,
                _ => {
                    return Err(Error::TokenMismatch {
                        expected: TokenType::Equal,
                        actual: self.prev().unwrap().token_type.clone(),
                        line: self.prev().unwrap().line,
                        col: self.prev().unwrap().start,
                    });
                }
            };

            let right = self.equality()?;

            return Ok(Expr::Logical(Logical {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let left = self.term()?;

        while self.check(TokenType::Less)
            || self.check(TokenType::LessEqual)
            || self.check(TokenType::Greater)
            || self.check(TokenType::GreaterEqual)
        {
            let op = match self.prev().unwrap().token_type {
                TokenType::Less => LogicalOp::Equal,
                TokenType::NotEqual => LogicalOp::NotEqual,
                _ => {
                    return Err(Error::TokenMismatch {
                        expected: TokenType::Equal,
                        actual: self.prev().unwrap().token_type.clone(),
                        line: self.prev().unwrap().line,
                        col: self.prev().unwrap().start,
                    });
                }
            };

            let right = self.comparison()?;
            return Ok(Expr::Logical(Logical {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let left = self.comparison()?;

        while self.check(TokenType::Equal) || self.check(TokenType::NotEqual) {
            let op = match self.prev().unwrap().token_type {
                TokenType::Equal => LogicalOp::Equal,
                TokenType::NotEqual => LogicalOp::NotEqual,
                _ => {
                    return Err(Error::TokenMismatch {
                        expected: TokenType::Equal,
                        actual: self.prev().unwrap().token_type.clone(),
                        line: self.prev().unwrap().line,
                        col: self.prev().unwrap().start,
                    });
                }
            };

            let right = self.comparison();
        }

        Ok(left)
    }

    fn consume(&mut self, expected: TokenType) -> Result<&Token, Error> {
        let curr = self.advance()?;

        if curr.token_type != expected {
            return Err(Error::TokenMismatch {
                expected: expected,
                actual: curr.token_type.clone(),
                line: curr.line,
                col: curr.start,
            });
        }

        Ok(curr)
    }

    fn advance(&mut self) -> Result<&Token, Error> {
        if self.is_at_end() {
            return Err(Error::EOS);
        }

        self.curr += 1;
        let curr = self.prev().unwrap();
        Ok(curr)
    }

    fn check(&mut self, expected: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        let curr = self.current().unwrap();
        if curr != expected {
            return false;
        }

        self.advance();
        return true;
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.curr)
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.curr - 1)
    }

    fn next(&self) -> Option<&Token> {
        self.tokens.get(self.curr + 1)
    }

    fn is_at_end(&self) -> bool {
        match self.current() {
            Some(curr) => curr != TokenType::EOS,
            _ => true,
        }
    }

    fn current(&self) -> Option<TokenType> {
        let curr = self.peek();
        match curr {
            Some(token) => Some(token.token_type.clone()),
            None => None,
        }
    }
}
