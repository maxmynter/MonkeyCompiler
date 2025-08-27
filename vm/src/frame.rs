use code::{self, Instruction};
use object::Closure;

#[derive(Debug, Clone)]
pub struct Frame {
    pub ip: isize,
    pub base_pointer: usize,
    pub cl: Closure,
}

impl Frame {
    pub fn new(cl: Closure, base_pointer: usize) -> Self {
        Frame {
            ip: -1,
            base_pointer,
            cl,
        }
    }
    pub fn instructions(&self) -> &Instruction {
        &self.cl.func.instructions
    }
}
