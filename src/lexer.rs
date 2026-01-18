use core::panic;
use std::{fs::File, io::Read, process::ExitCode, ptr::read};

#[derive(Debug, PartialEq, Clone, Copy)]
enum NumberType {
    Float(f32),
    Int(i32),
}

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    Identifier(String),
    String(String),
    Number(NumberType),

    // Keywords
    Class,
    Variable,
    Function,
    For,
    While,
    If,
    Else,
    Or,
    And,
    Not,
    True,
    False,
    Return,

    // Operators
    Plus,
    Minus,
    Divide,
    Multiply,

    // Boolean Operators
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Brackets/Parenthesises
    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenCurlyBrace,
    CloseCurlyBrace,

    Colon,
    SemiColon,

    EOS,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub start: usize,
    pub end: usize,
    pub pos: usize,
}

#[derive(Debug)]
pub struct Lexer {
    pub code: String,
    pub tokens: Vec<Token>,
    pub line: usize,
    pub pos: usize,
    pub col: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, start: usize, end: usize, pos: usize) -> Token {
        Token {
            token_type: token_type,
            line,
            start,
            end,
            pos,
        }
    }

    pub fn from_lexer(token_type: TokenType, lexer: &Lexer) -> Token {
        Token::new(token_type, lexer.line, lexer.col, lexer.col + 1, lexer.pos)
    }
}

impl Lexer {
    pub fn read(file: &str) -> Lexer {
        let mut code = String::new();
        File::open(file)
            .expect("Expected valid file path")
            .read_to_string(&mut code)
            .expect("Can't convert file into String");

        let mut code = code.trim().to_string();

        let mut lexer = Lexer {
            code,
            tokens: Vec::new(),
            line: 1,
            col: 1,
            pos: 0,
        };

        lexer.parse();
        lexer
    }

    fn parse(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '+' => self.create_token(TokenType::Plus),
                '-' => self.create_token(TokenType::Minus),
                '/' => self.create_token(TokenType::Divide),
                '*' => self.create_token(TokenType::Multiply),
                '(' => self.create_token(TokenType::OpenParenthesis),
                ')' => self.create_token(TokenType::CloseParenthesis),
                '{' => self.create_token(TokenType::OpenCurlyBrace),
                '}' => self.create_token(TokenType::CloseCurlyBrace),
                '[' => self.create_token(TokenType::OpenBracket),
                ']' => self.create_token(TokenType::CloseBracket),
                ';' => self.create_token(TokenType::SemiColon),
                ':' => self.create_token(TokenType::Colon),

                '=' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };

                    let token = Token::new(token_type, self.line, start, self.col, self.pos);
                    self.tokens.push(token);
                }

                '!' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::NotEqual
                    } else {
                        TokenType::Not
                    };

                    let token = Token::new(token_type, self.line, start, self.col, self.pos);
                    self.tokens.push(token);
                }

                '>' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    };

                    let token = Token::new(token_type, self.line, start, self.col, self.pos);
                    self.tokens.push(token);
                }

                '<' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };

                    let token = Token::new(token_type, self.line, start, self.col, self.pos);
                    self.tokens.push(token);
                }

                '0'..='9' => {
                    let digit = self.number();
                    self.tokens.push(digit);
                }

                '\"' => {
                    let str = self.string();
                    self.tokens.push(str);
                }

                c if c.is_alphabetic() => {
                    let token = self.identifier();
                    self.tokens.push(token);
                }

                '\n' => {
                    self.line += 1;
                    self.col = 0;
                    self.advance();
                }

                '\r' | '\t' | ' ' | _ => {
                    self.advance();
                }

                
            };
        }

        self.tokens.push(Token::new(
            TokenType::EOS,
            self.line,
            self.col,
            self.col + 1,
            self.pos,
        ));
    }

    fn number(&mut self) -> Token {
        let start = self.pos;
        let start_col = self.col;

        while self.is_numeric() {
            self.advance();
        }

        let num = match Self::get_number(&self.code[start..self.pos]) {
            Some(num) => num,
            None => {
                println!(
                    "Line {}, Col {}: Can't parse number. There are too many periods",
                    self.line, self.col
                );
                std::process::exit(1);
            }
        };

        Token::new(
            TokenType::Number(num),
            self.line,
            start_col,
            self.col,
            self.pos,
        )
    }

    fn identifier(&mut self) -> Token {
        let start = self.pos;
        let start_col = self.col;

        while self.is_character() {
            self.advance();
        }

        let keyword = String::from(&self.code[start..self.pos]);
        let token_type = match keyword.as_str() {
            "class" => TokenType::Class,
            "fun" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "ret" => TokenType::Return,
            "var" => TokenType::Variable,
            _ => TokenType::Identifier(keyword),
        };

        Token::new(token_type, self.line, start_col, self.col, self.pos)
    }

    fn string(&mut self ) -> Token {
        self.advance();
        let start = self.pos;
        let start_col = self.col;

        while self.peek().is_some() && self.peek().unwrap() != '\"' {
            self.advance();
        }
        
        let str = String::from(&self.code[start..self.pos]);
        self.advance();
        
        Token::new(TokenType::String(str), self.line, start_col, self.col, self.pos)
    }
    fn get_number(s: &str) -> Option<NumberType> {
        if let Ok(i) = s.parse::<i32>() {
            // inferred as isize from next line
            Some(NumberType::Int(i))
        } else if let Ok(f) = s.parse::<f32>() {
            Some(NumberType::Float(f))
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.code.chars().nth(self.pos);
        self.pos += 1;
        self.col += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.get_char(self.pos)
    }

    fn next(&self) -> Option<char> {
        self.get_char(self.pos + 1)
    }

    fn get_char(&self, pos: usize) -> Option<char> {
        self.code.chars().nth(pos)
    }

    fn match_token(&mut self, token_type: char) -> bool {
        let curr = self.advance();
        let next = self.peek();

        if next.is_none() || next.unwrap() != token_type {
            return false;
        }

        self.advance();
        true
    }

    fn is_numeric(&self) -> bool {
        let c = self.peek();

        match c {
            Some(val) => val.is_numeric() || val == '.',
            None => false,
        }
    }

    fn is_character(&self) -> bool {
        let c = self.peek();

        match c {
            Some(val) => (!val.is_whitespace() && val.is_alphanumeric()) || val == '_',
            None => false,
        }
    }

    fn create_token(&mut self, token_type: TokenType) {
        let token = Token::from_lexer(token_type, &self);
        self.tokens.push(token);
        self.advance();
    }
}
