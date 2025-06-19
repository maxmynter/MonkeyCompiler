use code::{Opcode, make};

#[test]
fn test_make() {
    struct Test {
        op: Opcode,
        operands: Vec<isize>,
        expected: Vec<u8>,
    };
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
