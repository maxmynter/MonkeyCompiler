use ast::Node;
use code::Instructions;
use object::Object;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&self, node: impl Node) -> Result<(), String> {
        // TODO
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}
