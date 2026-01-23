use crate::{compiler::Chunk, lexer::Lexer};

struct Pho;

impl Pho {
    pub fn run(file: &str) {
        let lexer = Lexer::read(file);
    }
}
