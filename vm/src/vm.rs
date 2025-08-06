mod frame;

use std::{collections::HashMap, usize};

use code::{Instruction, Opcode, read_uint8, read_uint16};
use compiler::Bytecode;
use frame::Frame;
use object::{FALSE, HashKey, HashPair, NULL, Object, ObjectTraits, TRUE};

pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

#[derive(Debug)]
pub enum VMError {
    StackOverflow,
    UnkownOpCode,
    PopFromEmptyStack,
    UnknownOpForOperands { msg: String },
    UnkownOperator { msg: String },
    UnHashable { msg: String },
    IndexedUnindexable,
}

pub struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    sp: usize,
    globals: Vec<Object>,
    frames: Vec<Option<Frame>>,
    frames_index: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let main_fn = Object::CompiledFunction {
            instructions: bytecode.instructions,
            num_locals: 0,
        };
        let main_frame = frame::Frame::new(main_fn, 0);
        let mut frames = vec![None; MAX_FRAMES];
        frames[0] = Some(main_frame);

        Self {
            constants: bytecode.constants,
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
            globals: vec![Object::Null; GLOBALS_SIZE],
            frames,
            frames_index: 1,
        }
    }

    pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Object>) -> Self {
        let main_fn = Object::CompiledFunction {
            instructions: bytecode.instructions,
            num_locals: 0,
        };
        let main_frame = frame::Frame::new(main_fn, 0);
        let mut frames = vec![None; MAX_FRAMES];
        frames[0] = Some(main_frame);

        Self {
            constants: bytecode.constants,
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
            globals,
            frames,
            frames_index: 1,
        }
    }

    pub fn current_frame(&mut self) -> &mut Frame {
        self.frames[self.frames_index - 1].as_mut().unwrap()
    }

    pub fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frames_index] = Some(frame);
        self.frames_index += 1;
    }

    pub fn pop_frame(&mut self) -> Option<Frame> {
        self.frames_index -= 1;

        self.frames[self.frames_index].take()
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
    pub fn get_globals(&self) -> Vec<Object> {
        self.globals.clone()
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

    fn execute_binary_string_op(
        &mut self,
        op: Opcode,
        left: String,
        right: String,
    ) -> Result<(), VMError> {
        if Opcode::OpAdd == op {
            let mut result = left.clone();
            result.push_str(&right);
            self.push(Object::String { value: result })?;
            Ok(())
        } else {
            Err(VMError::UnknownOpForOperands {
                msg: format!("String Operands: {}, {}", left, right),
            })
        }
    }

    fn execute_binary_op(&mut self, op: Opcode) -> Result<(), VMError> {
        match (self.pop()?, self.pop()?) {
            (Object::Integer { value: right_value }, Object::Integer { value: left_value }) => {
                self.execute_binary_integer_op(op, left_value, right_value)
            }
            (Object::String { value: right_value }, Object::String { value: left_value }) => {
                self.execute_binary_string_op(op, left_value, right_value)
            }
            (left, right) => Err(VMError::UnknownOpForOperands {
                msg: format!("unkown operands: {:?}, {:?}", left, right),
            }),
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), VMError> {
        match (self.pop()?, self.pop()?) {
            (Object::Integer { value: right_value }, Object::Integer { value: left_value }) => {
                self.execute_integer_comparison(op, left_value, right_value)
            }
            (left, right) => match op {
                Opcode::OpEqual => self.push(native_bool_to_boolean_object(right == left)),

                Opcode::OpNotEqual => self.push(native_bool_to_boolean_object(right != left)),

                _ => Err(VMError::UnkownOperator {
                    msg: format!("unkown operator: {:?} ({:?}, {:?})", op, left, right),
                }),
            },
        }
    }

    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left_value: i64,
        right_value: i64,
    ) -> Result<(), VMError> {
        match op {
            Opcode::OpEqual => self.push(native_bool_to_boolean_object(left_value == right_value)),
            Opcode::OpNotEqual => {
                self.push(native_bool_to_boolean_object(left_value != right_value))
            }
            Opcode::OpGreaterThan => {
                self.push(native_bool_to_boolean_object(left_value > right_value))
            }
            _ => Err(VMError::UnkownOperator {
                msg: format!("unkonwn operator {:?}", op),
            }),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), VMError> {
        let operator = self.pop()?;
        match operator {
            TRUE => self.push(FALSE),
            FALSE => self.push(TRUE),
            NULL => self.push(TRUE),
            _ => self.push(FALSE),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), VMError> {
        match self.pop()? {
            Object::Integer { value } => self.push(Object::Integer { value: -value }),
            _ => Err(VMError::UnkownOperator {
                msg: "unsupported type for negation".to_string(),
            }),
        }
    }

    fn build_array(&mut self, start_index: usize, end_index: usize) -> Result<Object, VMError> {
        let mut elements = vec![Object::Null; end_index - start_index];
        let mut i = start_index;
        while i < end_index {
            elements[i - start_index] = self.stack[i].clone();
            i += 1;
        }
        Ok(Object::Array { elements })
    }

    fn build_hash(&mut self, start_index: usize, end_index: usize) -> Result<Object, VMError> {
        let mut hashed_pairs: HashMap<HashKey, HashPair> = HashMap::new();
        for i in (start_index..end_index).step_by(2) {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();
            let pair = HashPair {
                key: key.clone(),
                value,
            };
            let hash_key = key.hash().map_err(|e| VMError::UnHashable {
                msg: format!("cannot hash: {:?}, error: {:?}", key, e),
            })?;
            hashed_pairs.insert(hash_key, pair);
        }
        Ok(Object::Hash {
            pairs: hashed_pairs,
        })
    }

    fn execute_array_index(&mut self, elements: Vec<Object>, idx: i64) -> Result<(), VMError> {
        if idx < 0 || idx >= elements.len() as i64 {
            self.push(NULL)
        } else {
            self.push(elements[idx as usize].clone())
        }
    }

    fn execute_hash_index(
        &mut self,
        hash: &HashMap<HashKey, HashPair>,
        index: Object,
    ) -> Result<(), VMError> {
        let hashkey = index.hash().map_err(|_| VMError::UnHashable {
            msg: "Cannot use unhashable hash key".to_string(),
        })?;
        if let Some(pair) = hash.get(&hashkey) {
            self.push(pair.value.clone())
        } else {
            self.push(NULL)
        }
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<(), VMError> {
        match (left, &index) {
            (Object::Array { elements }, Object::Integer { value }) => {
                self.execute_array_index(elements, *value)
            }
            (Object::Hash { pairs }, _) => self.execute_hash_index(&pairs, index),
            _ => Err(VMError::IndexedUnindexable),
        }
    }

    pub fn run(&mut self) -> Result<(), VMError> {
        let mut ins: &Instruction;
        let mut op: Opcode;

        while self.current_frame().ip < self.current_frame().instructions.len() as isize - 1 {
            self.current_frame().ip += 1;
            let ip = self.current_frame().ip as usize;
            ins = &self.current_frame().instructions;
            op = Opcode::from_u8(ins[ip]).unwrap();

            match op {
                Opcode::OpConstant => {
                    let const_index = code::read_uint16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
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
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
                    self.execute_comparison(op)?;
                }
                Opcode::OpBang => {
                    self.execute_bang_operator()?;
                }
                Opcode::OpMinus => {
                    self.execute_minus_operator()?;
                }
                Opcode::OpJump => {
                    let pos = read_uint16(&ins[ip + 1..]);
                    // Set to preceeding instruction since we increment at loop end
                    self.current_frame().ip = pos - 1;
                }
                Opcode::OpJumpNotTruthy => {
                    let pos = read_uint16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
                    let condition = self.pop()?;
                    if !object::is_truthy(condition) {
                        self.current_frame().ip = pos - 1;
                    }
                }
                Opcode::OpNull => {
                    self.push(NULL)?;
                }
                Opcode::OpSetGlobal => {
                    let global_index = read_uint16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
                    self.globals[global_index as usize] = self.pop()?;
                }
                Opcode::OpGetGlobal => {
                    let global_index = read_uint16(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
                    self.push(self.globals[global_index as usize].clone())?;
                }
                Opcode::OpArray => {
                    let num_elements = read_uint16(&ins[ip + 1..]) as usize;
                    self.current_frame().ip += 2;
                    let arr = self.build_array(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;
                    self.push(arr)?;
                }
                Opcode::OpHash => {
                    let num_elements = read_uint16(&ins[ip + 1..]) as usize;
                    self.current_frame().ip += 2;
                    let hash = self.build_hash(self.sp - num_elements, self.sp)?;
                    self.sp -= num_elements;
                    self.push(hash)?;
                }
                Opcode::OpIndex => {
                    let index = self.pop()?;
                    let left = self.pop()?;
                    self.execute_index_expression(left, index)?;
                }
                Opcode::OpCall => {
                    let func = self.stack[self.sp - 1].clone();
                    match func {
                        Object::CompiledFunction { num_locals, .. } => {
                            let frame = Frame::new(func, self.sp);
                            self.sp = frame.base_pointer + num_locals;
                            self.push_frame(frame);
                        }
                        _ => unreachable!(),
                    }
                }
                Opcode::OpReturnValue => {
                    let return_value = self.pop()?;
                    let frame = self
                        .pop_frame()
                        .expect("tried to pop frame from empty frame stack");
                    self.sp = frame.base_pointer - 1;
                    self.push(return_value)?;
                }
                Opcode::OpReturn => {
                    let frame = self
                        .pop_frame()
                        .expect("tried to pop frame from empty frame stack");
                    self.sp = frame.base_pointer - 1;
                    self.push(NULL)?;
                }
                Opcode::OpSetLocal => {
                    let local_index = read_uint8(&ins[ip + 1..]) as usize;
                    self.current_frame().ip += 1;
                    let base_pointer = self.current_frame().base_pointer;
                    self.stack[base_pointer + local_index] = self.pop()?;
                }
                Opcode::OpGetLocal => {
                    let local_index = read_uint8(&ins[ip + 1..]) as usize;
                    self.current_frame().ip += 1;
                    let base_pointer = self.current_frame().base_pointer;
                    self.push(self.stack[base_pointer + local_index].clone())?;
                }
            }
        }
        Ok(())
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input { TRUE } else { FALSE }
}
