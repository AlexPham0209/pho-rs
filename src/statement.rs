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
    pub expr: Expr,
}
