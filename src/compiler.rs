enum Opcode {
    Return,
}

struct Chunk {
    count: usize,
    capacity: usize,
    code: Vec<u32>,
}

impl Chunk {
    fn new() -> Self {
        Chunk {
            count: 0,
            capacity: 0,
            code: Vec::<u32>::new(),
        }
    }
}
