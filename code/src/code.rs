use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

#[derive(Clone)]
pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn as_string(&self) -> String {
        todo!()
    }
}

impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Instructions {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(u8)]
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Opcode {
    Constant,
}

#[derive(Clone)]
pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<usize>,
}

lazy_static! {
    pub static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        [(
            Opcode::Constant,
            Definition {
                name: "OpConstant".to_string(),
                operand_widths: vec![2],
            },
        )]
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

pub fn make(op: Opcode, operands: &[isize]) -> Instructions {
    let def = match DEFINITIONS.get(&op) {
        Some(def) => def,
        None => {
            return Instructions::new();
        }
    };
    let mut instruction = Instructions::new();
    instruction.push(op as u8);

    for (i, &operand) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let bytes = (operand as u16).to_be_bytes();
                instruction.extend_from_slice(&bytes);
            }
            _ => {
                unreachable!()
            }
        }
    }
    instruction
}
