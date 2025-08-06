use code::{self, Instruction};
use object::Object;

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: Instruction,
    pub ip: isize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: Object, base_pointer: usize) -> Self {
        match func {
            Object::CompiledFunction { instructions, .. } => Frame {
                instructions,
                ip: -1,
                base_pointer,
            },
            _ => panic!("Can only create a new stack frame for a CompiledFunction object"),
        }
    }
}
