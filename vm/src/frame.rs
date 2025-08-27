use code::{self, Instruction};
use object::Object;

#[derive(Debug, Clone)]
pub struct Frame {
    pub ip: isize,
    pub base_pointer: usize,
    pub cl: Object,
}

impl Frame {
    pub fn new(cl: Object, base_pointer: usize) -> Self {
        match &cl {
            Object::Closure { .. } => Frame {
                ip: -1,
                base_pointer,
                cl,
            },
            _ => panic!("new frame needs closure object"),
        }
    }
    pub fn instructions(&self) -> &Instruction {
        match &self.cl {
            Object::Closure { func, .. } => {
                &func.instructions
            }
            _ => panic!("closure should be a compiled function"),
        }
    }
}
