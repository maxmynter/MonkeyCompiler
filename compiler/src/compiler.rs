pub mod symbol_table;
use ast::{BlockStatement, Boolean, Expression, IfExpression, IntegerLiteral, Program, Statement};
use code::{Instructions, Opcode, make};
use object::Object;

const JUMP_PLACEHOLDER: isize = 9999;

pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
    pub last_instruction: Option<EmittedInstruction>,
    pub previous_instruction: Option<EmittedInstruction>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }

    pub fn compile(&mut self, node: impl Compilable) -> Result<(), String> {
        node.compile(self)
    }

    pub fn add_constant(&mut self, obj: Object) -> isize {
        self.constants.push(obj);
        (self.constants.len() - 1) as isize
    }

    pub fn add_instructions(&mut self, ins: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let prev = self.last_instruction.take();
        self.previous_instruction = prev;
        self.last_instruction = Some(EmittedInstruction {
            opcode: op,
            position: pos,
        });
    }

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instructions(ins);
        self.set_last_instruction(op, pos);
        pos
    }

    pub fn last_instruction_is_pop(&self) -> bool {
        if let Some(last) = &self.last_instruction {
            last.opcode == Opcode::OpPop
        } else {
            false
        }
    }

    pub fn remove_last_pop(&mut self) {
        self.instructions = self
            .instructions
            .slice(0..self.last_instruction.as_ref().unwrap().position);
        self.last_instruction = self.previous_instruction.take();
    }

    pub fn maybe_remove_last_pop(&mut self) {
        if self.last_instruction_is_pop() {
            self.remove_last_pop();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        self.instructions[pos..pos + new_instruction.len()].copy_from_slice(&new_instruction);
    }

    pub fn change_operand(&mut self, op_pos: usize, operand: isize) {
        let op = Opcode::from_u8(self.instructions[op_pos]).expect("Unkown instruction code");
        let new_instruction = make(op, &[operand]);
        self.replace_instruction(op_pos, new_instruction);
    }
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

pub trait Compilable {
    fn compile(&self, c: &mut Compiler) -> Result<(), String>;
}

impl Compilable for IntegerLiteral {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        let integer = Object::Integer { value: self.value };
        let pos = c.add_constant(integer);
        c.emit(Opcode::OpConstant, &[pos]);
        Ok(())
    }
}

impl Compilable for Program {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        for statement in &self.statements {
            statement.compile(c)?;
        }
        Ok(())
    }
}

impl Compilable for Statement {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        match self {
            Statement::Let { value, .. } => {
                let result = value.compile(c);
                Ok(())
            }
            Statement::Return { .. } => todo!(),
            Statement::Expression { value, .. } => {
                let result = value.compile(c);
                c.emit(Opcode::OpPop, &[]);
                result
            }
        }
    }
}

impl Compilable for Expression {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        match self {
            Expression::IntegerLiteral(int_lit) => int_lit.compile(c),
            Expression::InfixExpression(infix) => {
                let op = infix.operator.as_str();
                if op == "<" {
                    infix.right.compile(c)?;
                    infix.left.compile(c)?;
                    c.emit(Opcode::OpGreaterThan, &[]);
                } else {
                    infix.left.compile(c)?;
                    infix.right.compile(c)?;
                    match op {
                        "+" => c.emit(Opcode::OpAdd, &[]),
                        "-" => c.emit(Opcode::OpSub, &[]),
                        "*" => c.emit(Opcode::OpMul, &[]),
                        "/" => c.emit(Opcode::OpDiv, &[]),
                        ">" => c.emit(Opcode::OpGreaterThan, &[]),
                        "==" => c.emit(Opcode::OpEqual, &[]),
                        "!=" => c.emit(Opcode::OpNotEqual, &[]),
                        _ => {
                            return Err(format!("unkown operator, {}", infix.operator));
                        }
                    };
                };
                Ok(())
            }
            Expression::Boolean(Boolean { value, .. }) => {
                if *value {
                    c.emit(Opcode::OpTrue, &[])
                } else {
                    c.emit(Opcode::OpFalse, &[])
                };
                Ok(())
            }
            Expression::PrefixExpression(prefix) => {
                let op = prefix.operator.as_str();
                prefix.right.compile(c)?;
                match op {
                    "!" => c.emit(Opcode::OpBang, &[]),
                    "-" => c.emit(Opcode::OpMinus, &[]),
                    _ => {
                        return Err(format!("unkown operator {:?}", op));
                    }
                };
                Ok(())
            }
            Expression::IfExpression(IfExpression {
                condition,
                consequence,
                alternative,
                ..
            }) => {
                condition.compile(c)?;
                let jump_not_truthy_pos = c.emit(Opcode::OpJumpNotTruthy, &[JUMP_PLACEHOLDER]);

                let _ = consequence.compile(c);

                // We only need one Pop because it's a conditional
                // Only one path is executed.
                c.maybe_remove_last_pop();

                let jump_pos = c.emit(Opcode::OpJump, &[JUMP_PLACEHOLDER]);

                let after_consequence_pos = c.instructions.len() as isize;
                c.change_operand(jump_not_truthy_pos, after_consequence_pos);

                match alternative {
                    Some(alternative) => {
                        let _ = alternative.compile(c);
                        c.maybe_remove_last_pop();
                    }
                    None => {
                        let _ = c.emit(Opcode::OpNull, &[]);
                    }
                }

                let after_alternative_pos = c.instructions.len() as isize;
                c.change_operand(jump_pos, after_alternative_pos);

                Ok(())
            }

            _ => Err(format!("Not yer implemented: {:?}", self)), // TODO: add missing implementations
        }
    }
}

impl Compilable for BlockStatement {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        for s in &self.statements {
            s.compile(c)?;
        }
        Ok(())
    }
}
