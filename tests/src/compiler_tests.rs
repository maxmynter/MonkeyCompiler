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
    let cases = [Test {
        op: Opcode::Constant,
        operands: vec![65534],
        expected: vec![Opcode::Constant as u8, 255, 254],
    }];
    for tt in cases {
        let instruction = make(tt.op, &tt.operands);
        assert_eq!(instruction.len(), tt.expected.len());
        for (i, &expected_byte) in tt.expected.iter().enumerate() {
            assert_eq!(instruction[i], expected_byte);
        }
    }
}

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
    assert_eq!(actual.len(), concatenated.len());

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
    let tests = vec![CompilerTest {
        input: "1 + 2",
        expected_constants: vec![Object::Integer { value: 1 }, Object::Integer { value: 2 }],
        expected_instructions: vec![make(Opcode::Constant, &[0]), make(Opcode::Constant, &[1])],
    }];
    run_compiler_tests(tests);
}

fn run_compiler_tests(tests: Vec<CompilerTest>) {
    for tt in tests {
        let program = prepare_program_for_test(tt.input);

        let compiler = Compiler::new();

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
        make(Opcode::Constant, &[1]),
        make(Opcode::Constant, &[2]),
        make(Opcode::Constant, &[65535]),
    ];

    let expected = "0000 OpConstant 1
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

    let tests = [ReadOpTest {
        op: Opcode::Constant,
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
