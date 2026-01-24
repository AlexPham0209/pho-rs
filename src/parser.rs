use core::panic;

use crate::{
    expr::{
        self, BinaryOp, BinaryOpType, Expr, LogicalOp, LogicalOpType, Statement, UnaryOp,
        UnaryOpType, Variable,
    },
    lexer::{NumberType, Token, TokenType},
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

        if !self.check(TokenType::And) {
            let right: Expr = self.equality()?;
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
            let op = Parser::convert_to_logical_op(op);
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
            let op = Parser::convert_to_logical_op(op);
            let right = self.equality()?;

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
            let op = Parser::convert_to_binary_op(op);
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
            let op = Parser::convert_to_binary_op(op);
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

        if !self.check(TokenType::Exponent) {
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
            let op = match self.prev().unwrap().clone().token_type {
                TokenType::Sub => UnaryOpType::Neg,
                TokenType::Not => UnaryOpType::Not,
                _ => panic!("Expected Unary Operator ! or -"),
            };
            let value = self.unary()?;
            return Ok(Expr::UnaryOp(UnaryOp {
                op,
                value: Box::new(value),
            }));
        }

        self.primary()
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
        if self.is_at_end() {
            return Err(Error::EOS);
        }

        self.curr += 1;
        let curr: &Token = self.prev().unwrap();
        Ok(curr)
    }

    fn check_tokens(&mut self, tokens: &[TokenType]) -> bool {
        let valid = tokens.iter().cloned().any(|token| {
            if self.is_at_end() {
                return false;
            }

            let curr = self.current().unwrap();
            curr == token
        });

        if valid {
            let _ = self.advance();
        }

        return valid;
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

    pub fn convert_to_logical_op(token: &Token) -> LogicalOpType {
        match token.token_type {
            TokenType::And => LogicalOpType::And,
            TokenType::Or => LogicalOpType::Or,
            TokenType::Less => LogicalOpType::Less,
            TokenType::LessEqual => LogicalOpType::LessEqual,
            TokenType::Greater => LogicalOpType::Greater,
            TokenType::GreaterEqual => LogicalOpType::GreaterEqual,
            _ => panic!("Expected logical operator token"),
        }
    }

    pub fn convert_to_binary_op(token: &Token) -> BinaryOpType {
        match token.token_type {
            TokenType::Add => BinaryOpType::Add,
            TokenType::Sub => BinaryOpType::Sub,
            TokenType::Multiply => BinaryOpType::Multiply,
            TokenType::Exponent => BinaryOpType::Exponent,
            TokenType::Divide => BinaryOpType::Divide,
            TokenType::IntDivide => BinaryOpType::IntDivide,
            _ => panic!("Expected logical operator token"),
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
