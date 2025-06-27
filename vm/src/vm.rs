use code::{Instructions, Opcode, make};
use compiler::Bytecode;
use object::Object;

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }
    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last()
    }
    pub fn run(&mut self) -> Result<(), ()> {
        for ip in &self.instructions {
            let op = Opcode::from_u8(ip.clone());
            match op {
                _ => {
                    return Err(());
                }
            }
        }
        Ok(())
    }
}
