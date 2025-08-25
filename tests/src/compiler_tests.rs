#![allow(dead_code)]
use std::vec;

use crate::utils::prepare_program_for_test;
use code::{Instruction, Opcode, lookup, make, read_operands};
use compiler::Compiler;
use object::Object;

#[test]
fn test_make() {
    struct Test {
        op: Opcode,
        operands: Vec<isize>,
        expected: Vec<u8>,
    }
    let cases = [
        Test {
            op: Opcode::OpConstant,
            operands: vec![65534],
            expected: vec![Opcode::OpConstant as u8, 255, 254],
        },
        Test {
            op: Opcode::OpAdd,
            operands: Vec::new(),
            expected: vec![Opcode::OpAdd as u8],
        },
        Test {
            op: Opcode::OpGetLocal,
            operands: vec![255],
            expected: vec![Opcode::OpGetLocal as u8, 255],
        },
    ];
    for tt in cases {
        let instruction = make(tt.op, &tt.operands);
        assert_eq!(instruction.len(), tt.expected.len());
        for (i, &expected_byte) in tt.expected.iter().enumerate() {
            assert_eq!(instruction[i], expected_byte);
        }
    }
}

#[derive(Debug)]
struct CompilerTest {
    input: &'static str,
    expected_constants: Vec<Object>,
    expected_instructions: Vec<Instruction>,
}

fn concat_instructions(s: Vec<Instruction>) -> Instruction {
    let mut out = Instruction::new();
    for ins in s {
        out.extend(ins);
    }
    out
}

fn flatten_instructions(instructions: Vec<Instruction>) -> Instruction {
    let mut result = Instruction::new();
    for ins in instructions {
        result.extend(ins);
    }
    result
}

fn test_instruction(expected: &Instruction, actual: &Instruction) -> Result<(), String> {
    if actual.len() != expected.len() {
        panic!(
            "test_instructions failed: wrong instructions length\nwant={}\ngot={}",
            expected.as_string(),
            actual.as_string()
        );
    }

    for (i, &expected_byte) in expected.iter().enumerate() {
        assert_eq!(actual[i], expected_byte);
    }
    Ok(())
}

fn test_constants(expected: &[Object], actual: &[Object]) -> Result<(), String> {
    assert_eq!(expected.len(), actual.len());
    for (i, expected_constant) in expected.iter().enumerate() {
        match expected_constant {
            Object::Integer { value } => {
                if let Err(e) = test_integer_object(*value, &actual[i]) {
                    return Err(format!(
                        "constant: {} -- test_integer_object_failed: {}",
                        i, e
                    ));
                }
            }
            Object::String {
                value: expected_value,
            } => {
                if let Object::String {
                    value: actual_value,
                } = &actual[i]
                {
                    assert_eq!(expected_value, actual_value);
                } else {
                    return Err(format!(
                        "unexpected type. expected={:?}, got={:?} ",
                        expected_constant, &actual[i]
                    ));
                }
            }
            Object::CompiledFunction {
                instructions: expected_instructions,
                ..
            } => {
                if let Object::CompiledFunction {
                    instructions: actual_instructions,
                    ..
                } = &actual[i]
                {
                    test_instruction(expected_instructions, actual_instructions)?;
                } else {
                    panic!("Did not find compiled function instructions");
                }
            }
            _ => {
                return Err(format!(
                    "unsupported constant type: {:?}",
                    expected_constant
                ));
            }
        }
    }
    Ok(())
}

fn test_integer_object(expected: i64, actual: &Object) -> Result<(), String> {
    match actual {
        Object::Integer { value } => assert_eq!(*value, expected),
        _ => panic!("Is not integer object"),
    }
    Ok(())
}

#[test]
fn compiler_tests() {
    let tests = vec![
        CompilerTest {
            input: "1 + 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1;2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1 - 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1 * 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "2 / 1",
            expected_constants: vec![Object::Integer { value: 2 }, Object::Integer { value: 1 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpDiv, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "true",
            expected_constants: Vec::new(),
            expected_instructions: vec![make(Opcode::OpTrue, &[]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "false",
            expected_constants: Vec::new(),
            expected_instructions: vec![make(Opcode::OpFalse, &[]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "1 > 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1 < 2",
            expected_constants: vec![Object::Integer { value: 2 }, Object::Integer { value: 1 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpGreaterThan, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1 == 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "1 != 2",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "true == false",
            expected_constants: Vec::new(),
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "true != false",
            expected_constants: Vec::new(),
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpFalse, &[]),
                make(Opcode::OpNotEqual, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "-1",
            expected_constants: vec![Object::Integer { value: 1 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpMinus, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "!true",
            expected_constants: Vec::new(),
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpBang, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

fn run_compiler_tests(tests: Vec<CompilerTest>) {
    for tt in tests {
        let program = prepare_program_for_test(tt.input);

        let mut compiler = Compiler::new();

        if let Err(e) = compiler.compile(program) {
            panic!("compiler error: {}", e);
        }

        let bytecode = compiler.bytecode();

        let expected_flattened = concat_instructions(tt.expected_instructions);
        if let Err(e) = test_instruction(&expected_flattened, &bytecode.instructions) {
            panic!("testInstructions failed: {}", e);
        }
        if let Err(e) = test_constants(&tt.expected_constants, &bytecode.constants) {
            panic!("testConstants failed: {}", e);
        }
    }
}

#[test]
fn test_instructions_string() {
    let instructions = vec![
        make(Opcode::OpAdd, &[]),
        make(Opcode::OpGetLocal, &[1]),
        make(Opcode::OpConstant, &[2]),
        make(Opcode::OpConstant, &[65535]),
    ];

    let expected = "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
";
    let concatted = concat_instructions(instructions);
    assert_eq!(concatted.as_string(), expected);
}

#[test]
fn test_read_operands() {
    struct ReadOpTest<'a> {
        op: Opcode,
        operands: &'a [isize],
        bytes_read: isize,
    }

    let tests = [
        ReadOpTest {
            op: Opcode::OpConstant,
            operands: &[65535],
            bytes_read: 2,
        },
        ReadOpTest {
            op: Opcode::OpGetLocal,
            operands: &[255],
            bytes_read: 1,
        },
    ];

    for tt in tests {
        let instruction = make(tt.op, tt.operands);
        match lookup(tt.op) {
            Some(def) => {
                let (operands_read, _n) = read_operands(&def, instruction.slice(1..));
                for (i, want) in tt.operands.iter().enumerate() {
                    assert_eq!(operands_read[i], *want);
                }
            }
            None => panic!("could not lookup: {:?}", tt.op),
        }
    }
}

#[test]
fn test_conditionals() {
    let tests = vec![
        CompilerTest {
            input: "if (true) { 10 }; 3333;",
            expected_constants: vec![
                Object::Integer { value: 10 },
                Object::Integer { value: 3333 },
            ],
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpNotTruthy, &[10]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[11]),
                make(Opcode::OpNull, &[]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "if (true) { 10 } else { 20 }; 3333;",
            expected_constants: vec![
                Object::Integer { value: 10 },
                Object::Integer { value: 20 },
                Object::Integer { value: 3333 },
            ],
            expected_instructions: vec![
                make(Opcode::OpTrue, &[]),
                make(Opcode::OpJumpNotTruthy, &[10]),
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpJump, &[13]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let one = 1; let two = 2;",
            expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSetGlobal, &[1]),
            ],
        },
        CompilerTest {
            input: "let one = 1; one;",
            expected_constants: vec![Object::Integer { value: 1 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let one = 1; let two = one; two;",
            expected_constants: vec![Object::Integer { value: 1 }],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpSetGlobal, &[1]),
                make(Opcode::OpGetGlobal, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_string_expression() {
    let tests = vec![
        CompilerTest {
            input: "\"monkey\"",
            expected_constants: vec![Object::String {
                value: "monkey".to_string(),
            }],
            expected_instructions: vec![make(Opcode::OpConstant, &[0]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "\"mon\" + \"key\"",
            expected_constants: vec![
                Object::String {
                    value: "mon".to_string(),
                },
                Object::String {
                    value: "key".to_string(),
                },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_array_literals() {
    let tests = vec![
        CompilerTest {
            input: "[]",
            expected_constants: vec![],
            expected_instructions: vec![make(Opcode::OpArray, &[0]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "[1, 2, 3]",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 3 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: " [1 + 2, 3 - 4, 5 * 6]",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 3 },
                Object::Integer { value: 4 },
                Object::Integer { value: 5 },
                Object::Integer { value: 6 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_hash_literal() {
    let tests = vec![
        CompilerTest {
            input: "{}",
            expected_constants: Vec::new(),
            expected_instructions: vec![make(Opcode::OpHash, &[0]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "{1: 2, 3: 4, 5: 6}",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 3 },
                Object::Integer { value: 4 },
                Object::Integer { value: 5 },
                Object::Integer { value: 6 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpHash, &[6]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "{1: 2 + 3, 4: 5 * 6}",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 3 },
                Object::Integer { value: 4 },
                Object::Integer { value: 5 },
                Object::Integer { value: 6 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpConstant, &[5]),
                make(Opcode::OpMul, &[]),
                make(Opcode::OpHash, &[4]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_index_expression() {
    let tests = vec![
        CompilerTest {
            input: "[1, 2, 3][1 + 1]",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 3 },
                Object::Integer { value: 1 },
                Object::Integer { value: 1 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpArray, &[3]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpConstant, &[4]),
                make(Opcode::OpAdd, &[]),
                make(Opcode::OpIndex, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "{1: 2}[2 - 1]",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::Integer { value: 2 },
                Object::Integer { value: 1 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpHash, &[2]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpSub, &[]),
                make(Opcode::OpIndex, &[]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_functions() {
    let tests = vec![
        CompilerTest {
            input: "fn() { return 5 + 10 }",
            expected_constants: vec![
                Object::Integer { value: 5 },
                Object::Integer { value: 10 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![make(Opcode::OpConstant, &[2]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "fn() { 5 + 10 }",
            expected_constants: vec![
                Object::Integer { value: 5 },
                Object::Integer { value: 10 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![make(Opcode::OpConstant, &[2]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "fn() { 1; 2}",
            expected_constants: vec![
                Object::Integer { value: 1 },
                Object::Integer { value: 2 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![make(Opcode::OpConstant, &[2]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "fn() {}",
            expected_constants: vec![Object::CompiledFunction {
                instructions: flatten_instructions(vec![make(Opcode::OpReturn, &[])]),
                num_locals: 0,
                num_parameters: 0,
            }],
            expected_instructions: vec![make(Opcode::OpConstant, &[0]), make(Opcode::OpPop, &[])],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_compiler_scopes() {
    let mut compiler = Compiler::new();
    assert_eq!(compiler.scope_index, 0);

    let global_symbol_table = compiler.symbol_table.clone();

    compiler.emit(Opcode::OpMul, &[]);
    compiler.enter_scope();
    assert_eq!(compiler.scope_index, 1);
    compiler.emit(Opcode::OpSub, &[]);
    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1);
    let last = compiler.scopes[compiler.scope_index].last_instruction;
    assert_eq!(last.unwrap().opcode, Opcode::OpSub);

    assert_eq!(
        compiler.symbol_table.outer.as_ref().map(|b| b.as_ref()),
        Some(&global_symbol_table)
    );

    compiler.leave_scope();
    assert_eq!(compiler.scope_index, 0);

    assert_eq!(compiler.symbol_table, global_symbol_table);
    assert!(compiler.symbol_table.outer.is_none());

    compiler.emit(Opcode::OpAdd, &[]);
    assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2);
    let previous = compiler.scopes[compiler.scope_index].previous_instruction;
    assert_eq!(previous.unwrap().opcode, Opcode::OpMul);
}

#[test]
fn test_function_calls() {
    let tests = vec![
        CompilerTest {
            input: "fn() {24}();",
            expected_constants: vec![
                Object::Integer { value: 24 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpCall, &[0]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let noArg = fn() {24}; noArg();",
            expected_constants: vec![
                Object::Integer { value: 24 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpCall, &[0]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let oneArg = fn(a) { }; oneArg(24);",
            expected_constants: vec![
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![make(Opcode::OpReturn, &[])]),
                    num_locals: 0,
                    num_parameters: 0,
                },
                Object::Integer { value: 24 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpCall, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let manyArg = fn(a, b, c) { }; manyArg(24, 25, 26);",
            expected_constants: vec![
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![make(Opcode::OpReturn, &[])]),
                    num_locals: 0,
                    num_parameters: 0,
                },
                Object::Integer { value: 24 },
                Object::Integer { value: 25 },
                Object::Integer { value: 26 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpCall, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let oneArg = fn(a) { a }; oneArg(24);",
            expected_constants: vec![
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 1,
                    num_parameters: 0,
                },
                Object::Integer { value: 24 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpCall, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
            expected_constants: vec![
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpGetLocal, &[1]),
                        make(Opcode::OpPop, &[]),
                        make(Opcode::OpGetLocal, &[2]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 3,
                    num_parameters: 0,
                },
                Object::Integer { value: 24 },
                Object::Integer { value: 25 },
                Object::Integer { value: 26 },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpGetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpConstant, &[2]),
                make(Opcode::OpConstant, &[3]),
                make(Opcode::OpCall, &[3]),
                make(Opcode::OpPop, &[]),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_let_statement_scopes() {
    let tests = vec![
        CompilerTest {
            input: "let num = 55; fn() { num }",
            expected_constants: vec![
                Object::Integer { value: 55 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpGetGlobal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, &[0]),
                make(Opcode::OpSetGlobal, &[0]),
                make(Opcode::OpConstant, &[1]),
                make(Opcode::OpPop, &[]),
            ],
        },
        CompilerTest {
            input: "fn() {let num = 55;  num }",
            expected_constants: vec![
                Object::Integer { value: 55 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![make(Opcode::OpConstant, &[1]), make(Opcode::OpPop, &[])],
        },
        CompilerTest {
            input: "fn() { let a = 55; let b = 77; a + b }",
            expected_constants: vec![
                Object::Integer { value: 55 },
                Object::Integer { value: 77 },
                Object::CompiledFunction {
                    instructions: flatten_instructions(vec![
                        make(Opcode::OpConstant, &[0]),
                        make(Opcode::OpSetLocal, &[0]),
                        make(Opcode::OpConstant, &[1]),
                        make(Opcode::OpSetLocal, &[1]),
                        make(Opcode::OpGetLocal, &[0]),
                        make(Opcode::OpGetLocal, &[1]),
                        make(Opcode::OpAdd, &[]),
                        make(Opcode::OpReturnValue, &[]),
                    ]),
                    num_locals: 0,
                    num_parameters: 0,
                },
            ],
            expected_instructions: vec![make(Opcode::OpConstant, &[2]), make(Opcode::OpPop, &[])],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_builtins() {
    let tests = vec![CompilerTest {
        input: "len([]); push([], 1);",
        expected_constants: vec![Object::Integer { value: 1 }],
        expected_instructions: vec![
            make(Opcode::OpGetBuiltin, &[0]),
            make(Opcode::OpArray, &[0]),
            make(Opcode::OpCall, &[1]),
            make(Opcode::OpPop, &[]),
            make(Opcode::OpGetBuiltin, &[5]),
            make(Opcode::OpArray, &[0]),
            make(Opcode::OpConstant, &[0]),
            make(Opcode::OpCall, &[2]),
            make(Opcode::OpPop, &[]),
        ],
    }];
    run_compiler_tests(tests);
}
