#![allow(dead_code)]
use crate::utils::prepare_program_for_test;
use code::{Instructions, Opcode, lookup, make, read_operands};
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
    expected_instructions: Vec<Instructions>,
}

fn concat_instructions(s: Vec<Instructions>) -> Instructions {
    let mut out = Instructions::new();
    for ins in s {
        out.extend(ins);
    }
    out
}

fn test_instructions(expected: Vec<Instructions>, actual: &Instructions) -> Result<(), String> {
    let concatenated = concat_instructions(expected);
    if actual.len() != concatenated.len() {
        panic!(
            "test_instructions failed: wrong instructions length\nwant={}\ngot={}",
            concatenated.as_string(),
            actual.as_string()
        );
    }

    for (i, &expected_byte) in concatenated.iter().enumerate() {
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

        if let Err(e) = test_instructions(tt.expected_instructions, &bytecode.instructions) {
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
        make(Opcode::OpConstant, &[2]),
        make(Opcode::OpConstant, &[65535]),
    ];

    let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
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

    let tests = [ReadOpTest {
        op: Opcode::OpConstant,
        operands: &[65535],
        bytes_read: 2,
    }];

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
