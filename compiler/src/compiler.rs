use ast::{Boolean, Expression, IntegerLiteral, Program, Statement};
use code::{Instructions, Opcode};
use object::Object;

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
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

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> usize {
        let ins = code::make(op, operands);

        self.add_instructions(ins)
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
        c.emit(Opcode::Constant, &[pos]);
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
            Statement::Let { .. } => todo!(),
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
                infix.left.compile(c)?;
                infix.right.compile(c)?;
                match infix.operator.as_str() {
                    "+" => c.emit(Opcode::OpAdd, &[]),
                    "-" => c.emit(Opcode::OpSub, &[]),
                    "*" => c.emit(Opcode::OpMul, &[]),
                    "/" => c.emit(Opcode::OpDiv, &[]),
                    _ => {
                        return Err(format!("unkown operator, {}", infix.operator));
                    }
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
            _ => Err("Not yet implemented".to_string()), // TODO: add missing implementations
        }
    }
}
