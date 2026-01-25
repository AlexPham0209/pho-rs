use core::panic;
use std::{fs::File, io::Read, process::ExitCode, ptr::read};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberType {
    Float(f32),
    Int(i32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier,
    String(String),
    Number(NumberType),

    // Keywords
    Class,
    Variable,
    Function,

    For,
    While,

    If,
    Elif,
    Else,
    Switch,
    Case,

    Or,
    And,
    Not,
    True,
    False,

    Return,
    In,

    // Operators
    Add,
    Sub,
    Multiply,
    Exponent,
    Divide,
    IntDivide,

    // Assign operators
    AddAssign,
    SubAssign,
    MultiplyAssign,
    ExponentAssign,
    DivideAssign,
    IntDivideAssign,

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
    Comma,

    Dot,
    Range,
    RangeEqual,

    Null,

    EOS,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
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
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        start: usize,
        end: usize,
        pos: usize,
    ) -> Token {
        Token {
            token_type: token_type,
            lexeme,
            line,
            start,
            end,
            pos,
        }
    }

    pub fn from_lexer(token_type: TokenType, lexeme: String, lexer: &Lexer) -> Token {
        Token::new(
            token_type,
            lexeme,
            lexer.line,
            lexer.col,
            lexer.col + 1,
            lexer.pos,
        )
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

    pub fn from_string(code: &str) -> Lexer {
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
                '(' => self.create_token(TokenType::OpenParenthesis, c.to_string()),
                ')' => self.create_token(TokenType::CloseParenthesis, c.to_string()),
                '{' => self.create_token(TokenType::OpenCurlyBrace, c.to_string()),
                '}' => self.create_token(TokenType::CloseCurlyBrace, c.to_string()),
                '[' => self.create_token(TokenType::OpenBracket, c.to_string()),
                ']' => self.create_token(TokenType::CloseBracket, c.to_string()),
                ';' => self.create_token(TokenType::SemiColon, c.to_string()),
                ':' => self.create_token(TokenType::Colon, c.to_string()),
                ',' => self.create_token(TokenType::Comma, c.to_string()),

                '+' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::AddAssign
                    } else {
                        TokenType::Add
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
                    self.tokens.push(token);
                }

                '-' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::SubAssign
                    } else {
                        TokenType::Sub
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
                    self.tokens.push(token);
                }

                '/' => {
                    let start = self.col;
                    let curr = self.advance();
                    let next = self.peek();
                    let following = self.next();

                    let token_type = if let Some(next_val) = next
                        && let Some(following_val) = following
                        && next_val == '/'
                        && following_val == '='
                    {
                        self.advance();
                        self.advance();
                        TokenType::IntDivideAssign
                    } else {
                        match next {
                            Some('/') => {
                                self.advance();
                                TokenType::IntDivide
                            }
                            Some('=') => {
                                self.advance();
                                TokenType::DivideAssign
                            }
                            _ => TokenType::Divide,
                        }
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
                    self.tokens.push(token);
                }

                '*' => {
                    let start = self.col;
                    let curr = self.advance();
                    let next = self.peek();
                    let following = self.next();

                    let token_type = if let Some(next_val) = next
                        && let Some(following_val) = following
                        && next_val == '*'
                        && following_val == '='
                    {
                        self.advance();
                        self.advance();
                        TokenType::ExponentAssign
                    } else {
                        match next {
                            Some('*') => {
                                self.advance();
                                TokenType::Exponent
                            }
                            Some('=') => {
                                self.advance();
                                TokenType::MultiplyAssign
                            }
                            _ => TokenType::Multiply,
                        }
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
                    self.tokens.push(token);
                }

                '.' => {
                    let start = self.col;
                    let curr = self.advance();
                    let next = self.peek();
                    let following = self.next();

                    let token_type = if let Some(next_val) = next
                        && let Some(following_val) = following
                        && next_val == '.'
                        && following_val == '='
                    {
                        self.advance();
                        self.advance();
                        TokenType::RangeEqual
                    } else {
                        match next {
                            Some('.') => {
                                self.advance();
                                TokenType::Range
                            }
                            _ => TokenType::Dot,
                        }
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
                    self.tokens.push(token);
                }

                '=' => {
                    let start = self.col;
                    let match_token = self.match_token('=');

                    let token_type = if match_token {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
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

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
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

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
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

                    let token = Token::new(
                        token_type,
                        c.to_string(),
                        self.line,
                        start,
                        self.col,
                        self.pos,
                    );
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

                '\r' | '\t' | ' ' => {
                    self.advance();
                }

                _ => {
                    panic!("Invalid");
                }
            };
        }

        self.tokens.push(Token::new(
            TokenType::EOS,
            "EOS".to_string(),
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

        // Consume more if there is a dot
        if let Some(val) = self.peek()
            && val == '.'
        {
            self.advance();
            while self.is_numeric() {
                self.advance();
            }
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
            self.code[start..self.pos].to_string(),
            self.line,
            start_col,
            self.col,
            self.pos,
        )
    }

    fn identifier(&mut self) -> Token {
        let start = self.pos;
        let start_col = self.col;

        while self.is_character() || self.is_numeric() {
            self.advance();
        }

        let keyword = String::from(&self.code[start..self.pos]);
        let token_type = {
            use TokenType::*;
            match keyword.as_str() {
                "class" => Class,
                "fun" => Function,
                "if" => If,
                "elif" => Elif,
                "else" => Else,
                "for" => For,
                "while" => While,
                "and" => And,
                "or" => Or,
                "true" => True,
                "false" => False,
                "ret" => Return,
                "set" => Variable,
                "switch" => Switch,
                "case" => Case,
                "in" => In,
                "null" => Null,
                _ => Identifier,
            }
        };

        Token::new(
            token_type, keyword, self.line, start_col, self.col, self.pos,
        )
    }

    fn string(&mut self) -> Token {
        self.advance();
        let start = self.pos;
        let start_col = self.col;

        while self.peek().is_some() && self.peek().unwrap() != '\"' {
            self.advance();
        }

        let str = String::from(&self.code[start..self.pos]);
        self.advance();

        Token::new(
            TokenType::String(str.clone()),
            str,
            self.line,
            start_col,
            self.col,
            self.pos,
        )
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
            Some(val) => val.is_numeric(),
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

    fn create_token(&mut self, token_type: TokenType, lexeme: String) {
        let token = Token::from_lexer(token_type, lexeme, &self);
        self.tokens.push(token);
        self.advance();
    }
}
