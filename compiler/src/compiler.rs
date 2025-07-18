pub mod symbol_table;
use std::rc::Rc;

use ast::{
    ArrayLiteral, BlockStatement, Boolean, Expression, FunctionLiteral, HashLiteral, Identifier,
    IfExpression, IndexExpression, IntegerLiteral, Node, Program, Statement, StringLiteral,
};
use code::{Instruction, Opcode, make};
use object::Object;

use crate::symbol_table::SymbolTable;

const JUMP_PLACEHOLDER: isize = 9999;

#[derive(Clone, Copy)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

impl Default for EmittedInstruction {
    fn default() -> Self {
        EmittedInstruction {
            opcode: Opcode::OpNull,
            position: 0,
        }
    }
}

pub struct CompilationScope {
    pub instructions: Vec<Instruction>,
    pub last_instruction: Option<EmittedInstruction>,
    pub previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    fn new() -> Self {
        CompilationScope {
            instructions: Vec::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }
}

pub struct Compiler {
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,
    pub scopes: Vec<CompilationScope>,
    pub scope_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
            scopes: vec![CompilationScope::new()],
            scope_index: 0,
        }
    }

    pub fn new_with_state(constants: Vec<Object>, symbols: SymbolTable) -> Self {
        Compiler {
            constants,
            symbol_table: symbols,
            scopes: Vec::new(),
            scope_index: 0,
        }
    }

    pub fn compile(&mut self, node: impl Compilable) -> Result<(), String> {
        node.compile(self)
    }

    pub fn add_constant(&mut self, obj: Object) -> isize {
        self.constants.push(obj);
        (self.constants.len() - 1) as isize
    }

    pub fn add_instructions(&mut self, ins: Instruction) -> usize {
        let current_instructions = self.current_instructions();

        let pos = current_instructions.len();
        current_instructions.push(ins);

        self.scopes[self.scope_index].instructions = current_instructions.to_vec();
        pos
    }

    pub fn current_instructions(&mut self) -> &mut Vec<Instruction> {
        &mut self.scopes[self.scope_index].instructions
    }

    pub fn bytecode(&self) -> Bytecode {
        let mut instructions = Instruction::new();
        for instruction in &self.scopes[self.scope_index].instructions {
            instructions.extend(instruction.iter());
        }
        Bytecode {
            instructions,
            constants: self.constants.clone(),
        }
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let prev = self.scopes[self.scope_index].last_instruction.take();
        let last = Some(EmittedInstruction {
            opcode: op,
            position: pos,
        });

        self.scopes[self.scope_index].previous_instruction = prev;
        self.scopes[self.scope_index].last_instruction = last;
    }

    pub fn emit(&mut self, op: Opcode, operands: &[isize]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instructions(ins);
        self.set_last_instruction(op, pos);
        pos
    }

    pub fn last_instruction_is_pop(&self) -> bool {
        if let Some(last) = &self.scopes[self.scope_index].last_instruction {
            last.opcode == Opcode::OpPop
        } else {
            false
        }
    }

    pub fn remove_last_pop(&mut self) {
        let scopes = &mut self.scopes[self.scope_index];
        scopes.instructions =
            scopes.instructions[0..scopes.last_instruction.as_ref().unwrap().position].to_vec();
        scopes.last_instruction = scopes.previous_instruction.take();
    }

    pub fn maybe_remove_last_pop(&mut self) {
        if self.last_instruction_is_pop() {
            self.remove_last_pop();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instruction) {
        self.scopes[self.scope_index].instructions[pos] = new_instruction;
    }

    pub fn change_operand(&mut self, op_pos: usize, operand: isize) {
        let op = Opcode::from_u8(self.scopes[self.scope_index].instructions[op_pos][0])
            .expect("Unkown instruction code");
        let new_instruction = make(op, &[operand]);
        self.replace_instruction(op_pos, new_instruction);
    }

    pub fn enter_scope(&mut self) {
        let scope = CompilationScope {
            last_instruction: None,
            previous_instruction: None,
            instructions: vec![],
        };
        self.scopes.push(scope);
        self.scope_index += 1;
    }

    pub fn leave_scope(&mut self) -> Vec<Instruction> {
        let popped_scope = self.scopes.pop().expect("exected scope");
        self.scope_index -= 1;
        popped_scope.instructions
    }
}

pub struct Bytecode {
    pub instructions: Instruction, // Flattened Instruction vec
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

impl Compilable for FunctionLiteral {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        c.enter_scope();
        let body = Rc::unwrap_or_clone(self.body.clone());
        c.compile(body);
        let instructions = c.leave_scope();
        let compiled_fn = Object::CompiledFunction { instructions };
        let pos = c.add_constant(compiled_fn);
        c.emit(Opcode::OpConstant, &[pos]);
        Ok(())
    }
}

impl Compilable for StringLiteral {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        let string = Object::String {
            value: self.value.clone(),
        };
        let pos = c.add_constant(string);
        c.emit(Opcode::OpConstant, &[pos]);
        Ok(())
    }
}

impl Compilable for ArrayLiteral {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        for el in self.elements.iter() {
            el.compile(c)?;
        }
        c.emit(Opcode::OpArray, &[self.elements.len() as isize]);
        Ok(())
    }
}

impl Compilable for HashLiteral {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        for (k, v) in &self.pairs {
            k.compile(c)?;
            v.compile(c)?;
        }
        c.emit(Opcode::OpHash, &[(self.pairs.len() * 2) as isize]);
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
            Statement::Let { value, name, .. } => {
                let result = value.compile(c);
                let symbol = c.symbol_table.define((*name.value).clone());
                let symbol_index = symbol.index as isize;
                c.emit(Opcode::OpSetGlobal, &[symbol_index]);
                result
            }
            Statement::Return { value, .. } => {
                let result = value.compile(c);
                c.emit(Opcode::OpReturnValue, &[]);
                result
            }
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

                let after_consequence_pos = c.scopes[c.scope_index]
                    .instructions
                    .iter()
                    .fold(0, |acc, ins| acc + ins.len())
                    as isize;
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

                let after_alternative_pos = c.scopes[c.scope_index]
                    .instructions
                    .iter()
                    .fold(0, |acc, i| acc + i.len())
                    as isize;
                c.change_operand(jump_pos, after_alternative_pos);

                Ok(())
            }
            Expression::String(string_lit) => string_lit.compile(c),
            Expression::Identifier(Identifier { value, .. }) => {
                if let Some(symbol) = c.symbol_table.resolve(value) {
                    c.emit(Opcode::OpGetGlobal, &[symbol.index as isize]);
                    Ok(())
                } else {
                    Err(format!("unkown identifier: {}", value))
                }
            }
            Expression::Array(arr) => arr.compile(c),
            Expression::HashMap(hash) => hash.compile(c),
            Expression::Index(index) => index.compile(c),
            Expression::FunctionLiteral(function) => function.compile(c),

            _ => Err(format!("Not yet implemented: {:?}", self)), // TODO: add missing implementations
        }
    }
}

impl Compilable for IndexExpression {
    fn compile(&self, c: &mut Compiler) -> Result<(), String> {
        self.left.compile(c)?;
        self.index.compile(c)?;
        c.emit(Opcode::OpIndex, &[]);
        Ok(())
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
