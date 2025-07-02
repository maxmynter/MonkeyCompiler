use code::{Instructions, Opcode};
use compiler::Bytecode;
use object::Object;

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub enum VMError {
    StackOverflow,
    UnkownOpCode,
    PopFromEmptyStack,
    UnknownOpForOperands { msg: String },
}

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
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp > 0 {
            self.stack.get(self.sp - 1)
        } else {
            None
        }
    }

    pub fn push(&mut self, ob: Object) -> Result<(), VMError> {
        if self.sp >= STACK_SIZE {
            Err(VMError::StackOverflow)
        } else {
            self.stack[self.sp] = ob;
            self.sp += 1;
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Result<Object, VMError> {
        if self.sp <= 0 {
            Err(VMError::PopFromEmptyStack)
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp].clone())
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::from_u8(self.instructions[ip]).unwrap();
            match op {
                Opcode::Constant => {
                    let const_index = code::read_uint16(self.instructions.slice(ip + 1..));
                    ip += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                Opcode::OpAdd => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    let result = match (&left, &right) {
                        (
                            &Object::Integer { value: left_value },
                            &Object::Integer { value: right_value },
                        ) => left_value + right_value,
                        _ => {
                            return Err(VMError::UnknownOpForOperands {
                                msg: format!(
                                    "unkown operation add for operands: {:?}, {:?}",
                                    left, right
                                ),
                            });
                        }
                    };
                    let _ = self.push(Object::Integer { value: result });
                }
                _ => {
                    return Err(VMError::UnkownOpCode);
                }
            }
            ip += 1;
        }
        Ok(())
    }
}
