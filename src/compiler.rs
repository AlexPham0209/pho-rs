use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(FromPrimitive)]
enum Opcode {
    Return = 1,
    Constant = 2,
}

pub struct Chunk {
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::<u8>::new(),
        }
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn disassemble(&self) -> String {
        let mut offset = 0;
        let code = self
            .code
            .iter()
            .copied()
            .map(|x| Chunk::disassemble_instruction(&mut offset, x))
            .collect::<Vec<String>>()
            .join("\n");

        std::format!("====\n{}\n====", code)
    }

    fn disassemble_instruction(offset: &mut usize, byte: u8) -> String {
        let prev = *offset;
        let instr = match FromPrimitive::from_u8(byte) {
            Some(Opcode::Return) => Chunk::simple_instruction(offset, "Return"),
            Some(Opcode::Constant) => Chunk::simple_instruction(offset, "Constant"),
            None => "Unknown Opcode",
        };

        std::format!("{:04}: {}", prev, instr)
    }

    fn simple_instruction<'a>(offset: &'a mut usize, str: &'a str) -> &'a str {
        *offset += 1;
        str
    }

}
