use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    fmt::Write,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction(Vec<u8>);

impl Default for Instruction {
    fn default() -> Self {
        Self::new()
    }
}

impl Instruction {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn as_string(&self) -> String {
        let mut out = String::new();
        let mut i = 0;
        while i < self.0.len() {
            let Some(opcode) = Opcode::from_u8(self.0[i]) else {
                writeln!(out, "ERROR: unknown opcode {}", self.0[i]).unwrap();
                i += 1;
                continue;
            };
            if let Some(def) = lookup(opcode) {
                let (operands, read) = read_operands(&def, self.slice(i + 1..));
                writeln!(out, "{:04} {}", i, self.fmt_instruction(&def, &operands)).unwrap();
                i += 1 + read;
            }
        }
        out
    }

    fn fmt_instruction(&self, def: &Definition, operands: &[isize]) -> String {
        let operand_count = def.operand_widths.len();
        if operands.len() != operand_count {
            return format!(
                "ERROR: operand len {} does not match defined {}",
                operands.len(),
                operand_count
            );
        }
        match operand_count {
            0 => def.name.to_string(),
            1 => format!("{} {}", def.name, operands[0]),
            2 => format!("{} {} {}", def.name, operands[0], operands[1]),
            _ => format!("ERROR: unhandled operandCount for {}", def.name),
        }
    }

    pub fn slice<R>(&self, range: R) -> Instruction
    where
        R: std::slice::SliceIndex<[u8], Output = [u8]>,
    {
        Instruction(self.0[range].to_vec())
    }
}

impl IntoIterator for Instruction {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Instruction {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl Deref for Instruction {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instruction {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(u8)]
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Opcode {
    OpConstant,
    OpAdd,
    OpPop,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJumpNotTruthy,
    OpJump,
    OpNull,
    OpGetGlobal,
    OpSetGlobal,
    OpArray,
    OpHash,
    OpIndex,
    OpCall,
    OpReturnValue,
    OpReturn,
    OpSetLocal,
    OpGetLocal,
    OpGetBuiltin,
    OpClosure,
}

impl Opcode {
    pub fn from_u8(code: u8) -> Option<Self> {
        match code {
            0 => Some(Opcode::OpConstant),
            1 => Some(Opcode::OpAdd),
            2 => Some(Opcode::OpPop),
            3 => Some(Opcode::OpSub),
            4 => Some(Opcode::OpMul),
            5 => Some(Opcode::OpDiv),
            6 => Some(Opcode::OpTrue),
            7 => Some(Opcode::OpFalse),
            8 => Some(Opcode::OpEqual),
            9 => Some(Opcode::OpNotEqual),
            10 => Some(Opcode::OpGreaterThan),
            11 => Some(Opcode::OpMinus),
            12 => Some(Opcode::OpBang),
            13 => Some(Opcode::OpJumpNotTruthy),
            14 => Some(Opcode::OpJump),
            15 => Some(Opcode::OpNull),
            16 => Some(Opcode::OpGetGlobal),
            17 => Some(Opcode::OpSetGlobal),
            18 => Some(Opcode::OpArray),
            19 => Some(Opcode::OpHash),
            20 => Some(Opcode::OpIndex),
            21 => Some(Opcode::OpCall),
            22 => Some(Opcode::OpReturnValue),
            23 => Some(Opcode::OpReturn),
            24 => Some(Opcode::OpSetLocal),
            25 => Some(Opcode::OpGetLocal),
            26 => Some(Opcode::OpGetBuiltin),
            27 => Some(Opcode::OpClosure),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<usize>,
}

lazy_static! {
    pub static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        [
            (
                Opcode::OpConstant,
                Definition {
                    name: "OpConstant",
                    operand_widths: vec![2],
                },
            ),
            (
                Opcode::OpAdd,
                Definition {
                    name: "OpAdd",
                    operand_widths: Vec::new(), // Doesn't have any operands
                },
            ),
            (
                Opcode::OpPop,
                Definition {
                    name: "OpPop",
                    operand_widths: Vec::new()
            }
        ),
            (
                Opcode::OpSub,
                Definition {
                    name: "OpSub",
                    operand_widths: Vec::new()
            }
        ),
            (
                Opcode::OpMul,
                Definition {
                    name: "OpMul",
                    operand_widths: Vec::new()
            }
        ),
            (
                Opcode::OpDiv,
                Definition {
                    name: "OpDiv",
                    operand_widths: Vec::new()
            }
        ),
            (
                Opcode::OpTrue,
                Definition {
                    name: "OpTrue",
                    operand_widths: Vec::new()
            }
        ),
            (
                Opcode::OpFalse,
                Definition {
                    name: "OpFalse",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpEqual,
                Definition {
                    name: "OpEqual",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpNotEqual,
                Definition {
                    name: "OpNotEqual",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpGreaterThan,
                Definition {
                    name: "OpGreaterThan",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpMinus,
                Definition {
                    name: "OpMinus",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpBang,
                Definition {
                    name: "OpBang",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpJumpNotTruthy,
                Definition {
                    name: "OpJumpNotTruthy",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpJump,
                Definition {
                    name: "OpJump",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpNull,
                Definition {
                    name: "OpNull",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpSetGlobal,
                Definition {
                    name: "OpSetGlobal",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpGetGlobal,
                Definition {
                    name: "OpGetGlobal",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpArray,
                Definition {
                    name: "OpArray",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpHash,
                Definition {
                    name: "OpHash",
                    operand_widths: vec![2]
            }
        ),
        (
                Opcode::OpIndex,
                Definition {
                    name: "OpIndex",
                    operand_widths: Vec::new(),
            }
        ),
        (
                Opcode::OpCall,
                Definition {
                    name: "OpCall",
                    operand_widths: vec![1]
            }
        ),
        (
                Opcode::OpReturnValue,
                Definition {
                    name: "OpReturnValue",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpReturn,
                Definition {
                    name: "OpReturn",
                    operand_widths: Vec::new()
            }
        ),
        (
                Opcode::OpSetLocal,
                Definition {
                    name: "OpSetLocal",
                    operand_widths: vec![1]
            }
        ),
        (
                Opcode::OpGetLocal,
                Definition {
                    name: "OpGetLocal",
                    operand_widths: vec![1]
            }
        ),
        (
                Opcode::OpGetBuiltin,
                Definition {
                    name: "OpGetBuiltin",
                    operand_widths: vec![1]
            }
        ),
        (
                Opcode::OpClosure,
                Definition {
                    name: "OpClosure",
                    operand_widths: vec![2,1]
            }
        ),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

pub fn lookup(op: Opcode) -> Option<Definition> {
    if let Some(def) = DEFINITIONS.get(&op) {
        Some(def.clone())
    } else {
        eprintln!("Undefined op code: {:?}", op);
        None
    }
}

pub fn make(op: Opcode, operands: &[isize]) -> Instruction {
    let def = DEFINITIONS
        .get(&op)
        .unwrap_or_else(|| panic!("unknown opcode {:?}", op));

    let mut instruction_len = 1;
    for &width in &def.operand_widths {
        instruction_len += width;
    }

    let mut instruction = vec![0u8; instruction_len];
    instruction[0] = op as u8;

    let mut offset = 1;
    for (i, &operand) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let bytes = (operand as u16).to_be_bytes();
                instruction[offset..offset + 2].copy_from_slice(&bytes);
            }
            1 => {
                instruction[offset] = operand as u8;
            }
            _ => {
                unreachable!()
            }
        }
        offset += width;
    }
    Instruction(instruction)
}

pub fn read_operands(def: &Definition, ins: Instruction) -> (Vec<isize>, usize) {
    let mut operands = vec![0_isize; def.operand_widths.len()];
    let mut offset = 0;

    for (i, &width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => operands[i] = read_uint16(&ins[offset..]),
            1 => operands[i] = read_uint8(&ins[offset..]),
            _ => unreachable!(),
        }
        offset += width;
    }
    (operands, offset)
}

pub fn read_uint16(ins: &[u8]) -> isize {
    u16::from_be_bytes([ins[0], ins[1]]) as isize
}

pub fn read_uint8(ins: &[u8]) -> isize {
    ins[0] as isize
}
