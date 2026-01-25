use core::panic;

use crate::{
    expr::{self, BinaryOp, BinaryOpType, Expr, LogicalOp, LogicalOpType, UnaryOp, UnaryOpType},
    lexer::{NumberType, Token, TokenType},
    statement::{Block, Return, Statement, Variable},
};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    curr: usize,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(TokenType),

    TokenMismatch {
        expected: TokenType,
        actual: TokenType,
        line: usize,
        col: usize,
    },

    StatementError {
        token: TokenType,
        line: usize,
        col: usize,
    },

    DeclarationError {
        token: TokenType,
        line: usize,
        col: usize,
    },

    LogicalOpError {
        token: TokenType,
        line: usize,
        col: usize,
    },
    BinaryOpError {
        token: TokenType,
        line: usize,
        col: usize,
    },
    UnaryOpError {
        token: TokenType,
        line: usize,
        col: usize,
    },
    EOS {
        token: TokenType,
        line: usize,
        col: usize,
    },
}

impl<'a> Parser<'a> {
    pub fn from_tokens(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser { tokens, curr: 0 }
    }
    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::<Statement>::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(declaration) => statements.push(declaration),
                Err(error) => todo!(),
            }
        }

        statements
    }

    fn show_errors() {}

    fn declaration(&mut self) -> Result<Statement, Error> {
        let token = match self.current() {
            Some(TokenType::Class) => todo!(),
            Some(TokenType::Function) => todo!(),
            Some(_) => self.statement(),
            None => Err(Error::DeclarationError {
                token: self.current().unwrap().clone(),
                line: self.peek().unwrap().line,
                col: self.peek().unwrap().start,
            }),
        };

        token
    }

    // fn class(&mut self) -> Result<Statement, Error> {
    //     self.advance();
    //     let identifier = self.consume(TokenType::Identifier)?;
    //     let lexeme = identifier.lexeme.clone();

    // }

    // fn for_statement(&mut self) -> Result<Statement, Error> {
    //     self.advance()?;
    //     let identifier = self.consume(TokenType::Identifier)?;
    //     let lexeme = identifier.lexeme.clone();
    //     self.consume(TokenType::In)?;
    //     let call = self.call()?;
    //     Ok(
    //         Statement::For(For {

    //         })
    //     )
    // }

    fn statement(&mut self) -> Result<Statement, Error> {
        match self.current() {
            Some(TokenType::Variable) => self.variable(),
            Some(TokenType::If) => todo!(),
            Some(TokenType::Switch) => todo!(),
            Some(TokenType::For) => todo!(),
            Some(TokenType::While) => todo!(),
            Some(TokenType::Identifier) => todo!(),
            Some(TokenType::OpenCurlyBrace) => self.block(),
            Some(TokenType::Return) => self.return_statement(),
            Some(token) => self.expression_statement(),
            Some(TokenType::EOS) | None => Err(Error::EOS {
                token: self.current().unwrap(),
                line: self.peek().unwrap().line,
                col: self.peek().unwrap().line,
            }),

            Some(_) => Err(Error::StatementError {
                token: self.current().unwrap(),
                line: self.peek().unwrap().line,
                col: self.peek().unwrap().line,
            }),
        }
    }

    fn variable(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        let identifier = self.consume(TokenType::Identifier)?;
        let lexeme = identifier.lexeme.clone();
        self.consume(TokenType::Equal)?;
        let expression = self.expression()?;

        Ok(Statement::Variable(Variable {
            identifier: lexeme,
            value: Box::new(expression),
        }))
    }

    fn block(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        let mut statements = Vec::<Statement>::new();

        while !self.check(TokenType::CloseCurlyBrace) {
            statements.push(self.statement()?);
        }

        Ok(Statement::Block(Block { statements }))
    }

    fn return_statement(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        let expr = self.expression()?;
        Ok(Statement::Return(Return { expr }))
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        Ok(Statement::Expression(self.expression()?))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        let or = self.or()?;
        Ok(or)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let left = self.and()?;

        if self.check(TokenType::Or) {
            let right: Expr = self.or()?;
            return Ok(Expr::LogicalOp(LogicalOp {
                op: LogicalOpType::Or,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let left = self.equality()?;

        if self.check(TokenType::And) {
            let right: Expr = self.and()?;
            return Ok(Expr::LogicalOp(LogicalOp {
                op: LogicalOpType::And,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let left = self.comparison()?;

        if self.check_tokens(&[TokenType::Equal, TokenType::NotEqual]) {
            let op = self.prev().unwrap();
            let op = Parser::convert_to_logical_op(op)?;
            let right = self.equality()?;

            return Ok(Expr::LogicalOp(LogicalOp {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let left = self.term()?;

        if self.check_tokens(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let op = self.prev().unwrap();
            let op = Parser::convert_to_logical_op(op)?;
            let right = self.comparison()?;

            return Ok(Expr::LogicalOp(LogicalOp {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let left = self.factor()?;

        if self.check_tokens(&[TokenType::Add, TokenType::Sub]) {
            let op = self.prev().unwrap();
            let op = Parser::convert_to_binary_op(op)?;
            let right = self.term()?;

            return Ok(Expr::BinaryOp(BinaryOp {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let left = self.exponent()?;

        if self.check_tokens(&[TokenType::Multiply, TokenType::Divide, TokenType::IntDivide]) {
            let op = self.prev().unwrap();
            let op = Parser::convert_to_binary_op(op)?;
            let right = self.factor()?;
            return Ok(Expr::BinaryOp(BinaryOp {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn exponent(&mut self) -> Result<Expr, Error> {
        let left = self.unary()?;

        if self.check(TokenType::Exponent) {
            let right: Expr = self.exponent()?;
            return Ok(Expr::BinaryOp(BinaryOp {
                op: BinaryOpType::Exponent,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.check_tokens(&[TokenType::Sub, TokenType::Not]) {
            let prev = self.prev().unwrap().clone();
            let op = match &prev.token_type {
                TokenType::Sub => UnaryOpType::Neg,
                TokenType::Not => UnaryOpType::Not,
                token => {
                    return Err(Error::BinaryOpError {
                        token: prev.token_type.clone(),
                        line: prev.line,
                        col: prev.start,
                    });
                }
            };
            let value = self.unary()?;
            return Ok(Expr::UnaryOp(UnaryOp {
                op,
                value: Box::new(value),
            }));
        }

        let primary = self.primary()?;
        Ok(primary)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        use expr::Literal::*;
        match &self.advance()?.token_type {
            TokenType::String(str) => Ok(Expr::Literal(String(str.clone()))),
            TokenType::Number(NumberType::Int(num)) => Ok(Expr::Literal(Int(num.clone()))),
            TokenType::Number(NumberType::Float(float)) => Ok(Expr::Literal(Float(float.clone()))),
            TokenType::True => Ok(Expr::Literal(Bool(true))),
            TokenType::False => Ok(Expr::Literal(Bool(false))),
            TokenType::OpenParenthesis => {
                let expr = self.expression();
                self.consume(TokenType::CloseParenthesis)?;
                expr
            }
            token => Err(Error::UnexpectedToken(token.clone())),
        }
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
        if !self.is_at_end() {
            self.curr += 1;
        }

        let curr: &Token = self.prev().unwrap();
        Ok(curr)
    }

    fn check_tokens(&mut self, tokens: &[TokenType]) -> bool {
        if tokens
            .iter()
            .cloned()
            .any(|token| !self.is_at_end() && self.current().unwrap() == token)
        {
            self.advance();
            return true;
        }

        false
    }

    fn check(&mut self, expected: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        let curr = self.current().unwrap();
        if curr != expected {
            return false;
        }

        let _ = self.advance();
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
            Some(curr) => curr == TokenType::EOS,
            _ => false,
        }
    }

    pub fn convert_to_logical_op(token: &Token) -> Result<LogicalOpType, Error> {
        match token.token_type {
            TokenType::And => Ok(LogicalOpType::And),
            TokenType::Or => Ok(LogicalOpType::Or),
            TokenType::Less => Ok(LogicalOpType::Less),
            TokenType::LessEqual => Ok(LogicalOpType::LessEqual),
            TokenType::Greater => Ok(LogicalOpType::Greater),
            TokenType::GreaterEqual => Ok(LogicalOpType::GreaterEqual),
            _ => {
                return Err(Error::UnaryOpError {
                    token: token.token_type.clone(),
                    line: token.line,
                    col: token.start,
                });
            }
        }
    }

    pub fn convert_to_binary_op(token: &Token) -> Result<BinaryOpType, Error> {
        match token.token_type {
            TokenType::Add => Ok(BinaryOpType::Add),
            TokenType::Sub => Ok(BinaryOpType::Sub),
            TokenType::Multiply => Ok(BinaryOpType::Multiply),
            TokenType::Exponent => Ok(BinaryOpType::Exponent),
            TokenType::Divide => Ok(BinaryOpType::Divide),
            TokenType::IntDivide => Ok(BinaryOpType::IntDivide),
            _ => {
                return Err(Error::BinaryOpError {
                    token: token.token_type.clone(),
                    line: token.line,
                    col: token.start,
                });
            }
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

mod tests {
    use super::*;
    use crate::parser::tests;

    #[test]
    fn check_test() {
        let tokens = vec![
            Token::new(TokenType::Add, "+".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Sub, "-".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Multiply, "*".to_string(), 0, 0, 0, 0),
        ];
        let mut parser = Parser::from_tokens(&tokens);
        assert!(parser.check(TokenType::Add));
        assert!(parser.check(TokenType::Sub));
        assert!(parser.check(TokenType::Multiply));
        // parser.check_tokens(&[TokenType::])
    }

    #[test]
    fn check_tokens_test() {
        let tokens = vec![
            Token::new(TokenType::Add, "+".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Sub, "-".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Multiply, "*".to_string(), 0, 0, 0, 0),
        ];
        let mut parser = Parser::from_tokens(&tokens);
        assert!(parser.check_tokens(&[TokenType::Add, TokenType::Sub, TokenType::Multiply]));
        assert!(parser.check_tokens(&[TokenType::Add, TokenType::Sub, TokenType::Multiply]));
        assert!(!parser.check_tokens(&[TokenType::Add, TokenType::Sub]));
        assert!(parser.check_tokens(&[TokenType::Multiply]));
    }

    #[test]
    fn current_advance_test() {
        let tokens = vec![
            Token::new(TokenType::Add, "+".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Sub, "-".to_string(), 0, 0, 0, 0),
            Token::new(TokenType::Multiply, "*".to_string(), 0, 0, 0, 0),
        ];
        let mut parser = Parser::from_tokens(&tokens);
        let token = parser.current();
        parser.advance();
        assert_eq!(token.unwrap(), TokenType::Add);

        let token = parser.current();
        parser.advance();
        assert_eq!(token.unwrap(), TokenType::Sub);

        let token = parser.current();
        parser.advance();
        assert_eq!(token.unwrap(), TokenType::Multiply);
    }
}
