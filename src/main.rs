use crate::{compiler::Chunk, lexer::Lexer};

mod compiler;
mod lexer;
mod pho;
mod vm;

fn main() {
    let lexer = Lexer::read("bin\\test.txt");
    for c in &lexer.tokens {
        println!("{:?}", c);
    }
    
    let mut chunk = Chunk::new();
    chunk.write(1);
    chunk.write(1);
    chunk.write(1);
    chunk.write(1);

    println!("{}", chunk.disassemble());
}
