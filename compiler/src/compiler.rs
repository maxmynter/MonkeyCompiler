use ast::Node;
use code::Instructions;
use object::Object;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Object,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Object::Null,
        }
    }

    pub fn compile(node: impl Node) {
        todo!()
    }
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Object,
}
