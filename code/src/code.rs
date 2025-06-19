use lazy_static::lazy_static;
use std::collections::HashMap;

pub type Instructions = Vec<u8>;

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

pub fn make(op: Opcode, operands: &[isize]) -> Vec<u8> {
    let def = match DEFINITIONS.get(&op) {
        Some(def) => def,
        None => {
            return Vec::new();
        }
    };
    let mut instruction = Vec::new();
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
