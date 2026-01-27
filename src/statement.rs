use crate::expr::Expr;

#[derive(Debug)]
pub enum Statement {
    Class(Class),
    Variable(Variable),
    // For(For),
    // Body(Body),
    Block(Block),
    Expression(Expr),
    Return(Return),
    If(If),
    While(While),
    Function(Function)
}

#[derive(Debug)]
pub struct Class {
    variables: Vec<Variable>,
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String,
    pub parameters: Vec<String>,
    pub block: Box<Block>
}

#[derive(Debug)]
pub struct Variable {
    pub identifier: String,
    pub value: Box<Expr>,
}

// #[derive(Debug)]
// pub struct For {
//     pub identifier: String,
//     pub iterator: Call,
//     pub block: Block,
// }

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Return {
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<Expr>,
    pub block: Box<Block>,
    pub elifs: Vec<Elif>,
    pub else_block: Box<Option<Block>>,
}

#[derive(Debug)]
pub struct Elif {
    pub condition: Box<Expr>,
    pub block: Box<Block>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Box<Expr>,
    pub block: Box<Block>,
}
