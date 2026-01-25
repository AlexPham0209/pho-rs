use crate::{chunk::Chunk, lexer::Lexer, parser::Parser};

mod chunk;
mod compiler;
mod expr;
mod lexer;
mod parser;
mod pho;
mod vm;

fn main() {
    let lexer = Lexer::from_string("(6 - 2 ** -4 >= 5) and true");
    for c in &lexer.tokens {
        println!("{:?}", c);
    }

    let mut parser = Parser::from_tokens(&lexer.tokens);
    let tree = parser.parse();
    println!("{:?}", tree);

    // let mut chunk = Chunk::new();
    // chunk.write(1);
    // chunk.write(1);
    // chunk.write(1);
    // chunk.write(1);

    // println!("{}", chunk.disassemble());
}
