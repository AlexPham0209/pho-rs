use core::panic;

use crate::{
    expr::{self, Assignment, BinaryOp, BinaryOpType, Expr, LogicalOp, LogicalOpType, UnaryOp, UnaryOpType},
    lexer::{NumberType, Token, TokenType},
    statement::{self, Block, Elif, Function, If, Return, Statement, Variable, While},
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
            Some(TokenType::Function) => self.function(),
            Some(_) => self.statement(),
            None => Err(Error::DeclarationError {
                token: self.current().unwrap().clone(),
                line: self.peek().unwrap().line,
                col: self.peek().unwrap().start,
            }),
        };

        token
    }

    fn function(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        let identifier = self.consume(TokenType::Identifier)?;
        let lexeme = identifier.lexeme.clone();
        let mut parameters = Vec::<String>::new();

        // Parameters
        self.consume(TokenType::OpenParenthesis)?;
        if self.check(TokenType::Identifier) {
            parameters.push(self.prev().unwrap().lexeme.clone());

            while self.check(TokenType::Comma) {
                self.consume(TokenType::Identifier)?;
                let parameter = self.prev().unwrap().lexeme.clone();
                parameters.push(parameter);
            }
        }
        self.consume(TokenType::CloseParenthesis)?;
        
        let block = match self.block()? {
            Statement::Block(block) => block,
            _ => {
                return Err(Error::StatementError {
                    token: self.current().unwrap(),
                    line: self.peek().unwrap().line,
                    col: self.peek().unwrap().line,
                });
            }
        };

        Ok(Statement::Function(Function {
            identifier: lexeme,
            block: Box::new(block),
            parameters,
        }))



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
            Some(TokenType::If) => self.if_statement(),
            Some(TokenType::Switch) => todo!(),
            Some(TokenType::For) => todo!(),
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::OpenCurlyBrace) => self.block(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::EOS) | None => Err(Error::EOS {
                token: self.current().unwrap(),
                line: self.peek().unwrap().line,
                col: self.peek().unwrap().line,
            }),
            Some(_) => self.expression_statement(),
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
        self.consume(TokenType::OpenCurlyBrace)?;
        let mut statements = Vec::<Statement>::new();

        while !self.check(TokenType::CloseCurlyBrace) {
            statements.push(self.statement()?);
        }

        Ok(Statement::Block(Block { statements }))
    }

    fn if_statement(&mut self) -> Result<Statement, Error> {
        self.advance()?;

        // Condition
        self.consume(TokenType::OpenParenthesis)?;
        let condition = self.expression()?;
        self.consume(TokenType::CloseParenthesis)?;

        // Block
        let block = match self.block()? {
            Statement::Block(block) => block,
            _ => {
                return Err(Error::StatementError {
                    token: self.current().unwrap(),
                    line: self.peek().unwrap().line,
                    col: self.peek().unwrap().start,
                });
            }
        };

        // Elif blocks
        let mut elifs = Vec::<Elif>::new();
        while self.check(TokenType::Elif) {
            let elif_statement = self.elif_statement()?;
            elifs.push(elif_statement);
        }

        // Else blocks
        let else_block = if self.check(TokenType::Else) {
            let block = match self.block()? {
                Statement::Block(block) => block,
                _ => {
                    return Err(Error::StatementError {
                        token: self.current().unwrap(),
                        line: self.peek().unwrap().line,
                        col: self.peek().unwrap().start,
                    });
                }
            };
            Some(block)
        } else {
            None
        };

        Ok(Statement::If(If {
            condition: Box::new(condition),
            block: Box::new(block),
            elifs,
            else_block: Box::new(else_block),
        }))
    }

    fn elif_statement(&mut self) -> Result<Elif, Error> {
        self.consume(TokenType::OpenParenthesis)?;
        let condition = self.expression()?;
        self.consume(TokenType::CloseParenthesis)?;

        let block = match self.block()? {
            Statement::Block(block) => block,
            _ => {
                return Err(Error::StatementError {
                    token: self.current().unwrap(),
                    line: self.peek().unwrap().line,
                    col: self.peek().unwrap().line,
                });
            }
        };

        Ok(Elif {
            condition: Box::new(condition),
            block: Box::new(block),
        })
    }

    fn while_statement(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        self.consume(TokenType::OpenParenthesis)?;
        let expr = self.expression()?;
        self.consume(TokenType::CloseParenthesis)?;

        let block = match self.block()? {
            Statement::Block(block) => block,
            _ => {
                return Err(Error::StatementError {
                    token: self.current().unwrap(),
                    line: self.peek().unwrap().line,
                    col: self.peek().unwrap().line,
                });
            }
        };

        Ok(Statement::While(While {
            condition: Box::new(expr),
            block: Box::new(block),
        }))
    }

    fn return_statement(&mut self) -> Result<Statement, Error> {
        self.advance()?;
        let expr = self.expression()?;
        Ok(Statement::Return(Return {
            expr: Box::new(expr),
        }))
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        Ok(Statement::Expression(self.expression()?))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        let or = self.or()?;

        if self.check_tokens(&[
            TokenType::AddAssign,
            TokenType::SubAssign,
            TokenType::MultiplyAssign,
            TokenType::ExponentAssign,
            TokenType::DivideAssign,
            TokenType::IntDivide,
            TokenType::Equal,
        ]) {
            let op = self.prev().unwrap().token_type.clone(); 
            let value = self.expression()?;
            return Ok(
                Expr::Assignment(Assignment {
                    left: Box::new(or),
                    right: Box::new(value),
                    op,
                })
            )
        }
        Ok(or)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let left = self.and()?;

        if self.check(TokenType::Or) {
            let right: Expr = self.or()?;
            return Ok(Expr::LogicalOp(LogicalOp {
                op: TokenType::Or,
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
                op: TokenType::And,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let left = self.comparison()?;

        if self.check_tokens(&[TokenType::EqualEqual, TokenType::NotEqual]) {
            let op = self.prev().unwrap().token_type.clone();
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
            let op = self.prev().unwrap().token_type.clone();
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
            let op = self.prev().unwrap().token_type.clone();
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
            let op = self.prev().unwrap().token_type.clone();
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
                op: TokenType::Exponent,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.check_tokens(&[TokenType::Sub, TokenType::Not]) {
            let prev = self.prev().unwrap();
            let op = prev.token_type.clone();

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
            TokenType::Identifier => Ok(Expr::Literal(Identifier(
                self.prev().unwrap().lexeme.to_string(),
            ))),
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
