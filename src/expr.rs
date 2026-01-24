use crate::lexer::NumberType;

pub enum Statement {
    Class(Class),
    Variable(Variable),
}

pub enum Expr {
    Logical(Logical),
}

pub enum Literal {
    String(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Null,
}

pub enum LogicalOp {
    And,
    Or,
    Equal,
    NotEqual,
}

pub struct Class {
    variables: Vec<Variable>,
}

pub struct Variable {
    pub identifier: String,
    pub value: Box<Expr>,
}

pub struct Logical {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: LogicalOp,
}
