use num_derive::FromPrimitive;
use num_traits::FromPrimitive;


#[derive(FromPrimitive)]
enum Opcode {
    Return = 1,
}


pub struct Chunk {
    pub code: Vec::<u8>
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::<u8>::new()
        }
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn disassemble(&self) -> String {
        self.code.iter().copied().map(Chunk::disassemble_instruction).collect::<Vec<String>>().join("\n")
    }

    fn disassemble_instruction(byte: u8) -> String {
        let instr = match FromPrimitive::from_u8(byte) {
            Some(Opcode::Return) => "Return",
            None => "Unknown Instruction"
        };

        instr.to_string()
    }
}
