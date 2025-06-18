use std::collections::HashMap;

use lazy_static::lazy_static;

type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Opcode {
    Constant,
}

#[derive(Clone)]
pub struct Definition {
    pub name: String,
    pub operand_width: Vec<usize>,
}

lazy_static! {
    pub static ref DEFINITIONS: HashMap<Opcode, Definition> = {
        [(
            Opcode::Constant,
            Definition {
                name: "OpConstant".to_string(),
                operand_width: vec![2],
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
