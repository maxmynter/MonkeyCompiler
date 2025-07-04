use code::{Instructions, Opcode};
use compiler::Bytecode;
use object::Object;

const STACK_SIZE: usize = 2048;
const TRUE: Object = Object::Boolean { value: true };
const FALSE: Object = Object::Boolean { value: false };

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

    pub fn last_popped_stack_elem(&self) -> Option<&Object> {
        self.stack.get(self.sp)
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
        if self.sp == 0 {
            Err(VMError::PopFromEmptyStack)
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp].clone())
        }
    }

    fn execute_binary_integer_op(
        &mut self,
        op: Opcode,
        left_value: i64,
        right_value: i64,
    ) -> Result<(), VMError> {
        let result = match op {
            Opcode::OpAdd => left_value + right_value,
            Opcode::OpSub => left_value - right_value,
            Opcode::OpMul => left_value * right_value,
            Opcode::OpDiv => left_value / right_value,
            _ => {
                return Err(VMError::UnknownOpForOperands {
                    msg: format!(
                        "Unkown integer operation: {:?} for operand values {:?}, {:?}",
                        op, left_value, right_value
                    ),
                });
            }
        };
        self.push(Object::Integer { value: result })
    }

    fn execute_binary_op(&mut self, op: Opcode) -> Result<(), VMError> {
        match (self.pop()?, self.pop()?) {
            (Object::Integer { value: right_value }, Object::Integer { value: left_value }) => {
                self.execute_binary_integer_op(op, left_value, right_value)
            }
            (left, right) => Err(VMError::UnknownOpForOperands {
                msg: format!("unkown operands: {:?}, {:?}", left, right),
            }),
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
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                    self.execute_binary_op(op)?;
                }
                Opcode::OpPop => {
                    self.pop()?;
                }
                Opcode::OpTrue => {
                    self.push(TRUE)?;
                }
                Opcode::OpFalse => {
                    self.push(FALSE)?;
                }
                _ => {
                    unreachable!();
                }
            }
            ip += 1;
        }
        Ok(())
    }
}
