use crate::lexer::Lexer;

mod compiler;
mod lexer;
mod pho;
mod vm;

fn main() {
    println!("Hello, world!");
    let lexer = Lexer::read("bin\\test.txt");
    for c in &lexer.tokens {
        println!("{:?}", c);
    }
}
