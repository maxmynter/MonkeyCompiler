use code::{self, Instruction};
use object::Object;

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: Vec<Instruction>,
    pub ip: isize,
}

impl Frame {
    pub fn new(func: Object) -> Self {
        match func {
            Object::CompiledFunction { instructions } => Frame {
                instructions,
                ip: -1,
            },
            _ => panic!("Can only create a new stack frame for a CompiledFunction object"),
        }
    }

    fn instructions(self) -> Vec<Instruction> {
        self.instructions
    }
}
