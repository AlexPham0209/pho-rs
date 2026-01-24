use crate::lexer::NumberType;

#[derive(Debug)]
pub enum Statement {
    Class(Class),
    Variable(Variable),
}

#[derive(Debug)]
pub enum Expr {
    LogicalOp(LogicalOp),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    String(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Null,
}
#[derive(Debug)]
pub struct Class {
    variables: Vec<Variable>,
}

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub enum LogicalOpType {
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug)]
pub struct LogicalOp {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: LogicalOpType,
}

#[derive(Debug)]
pub enum BinaryOpType {
    Add,
    Sub,
    Multiply,
    Exponent,
    Divide,
    IntDivide,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: BinaryOpType,
}

#[derive(Debug)]
pub enum UnaryOpType {
    Neg,
    Not,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub op: UnaryOpType,
    pub value: Box<Expr>,
}
