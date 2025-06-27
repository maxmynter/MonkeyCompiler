use code::Instructions;
use compiler::Bytecode;
use object::Object;

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Object,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        todo!()
    }
    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last()
    }
}
