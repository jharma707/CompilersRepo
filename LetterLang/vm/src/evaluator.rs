use crate::letter_value::LetterValue;
use crate::letter_stack::LetterStack;

pub const OP_HALT: u8 = 0;
pub const OP_CONST: u8 = 1;

pub fn eval(
    byte_code: &Vec<u8>,
    constant_pool: &Vec<LetterValue>,
    stack: &Stack,
) {
    if byte_code.is_empty() { return }

    // the instruction counter
    let mut ic = 0;

    loop {
        let opcode = byte_code[ic]; // TODO: make a READ_BYTE macro
        dbg!(opcode);

        ic = ic + 1;
        match opcode {
            OP_HALT => return,
            OP_CONST => {
                let constant_index = byte_code[ic];
                let constant = &constant_pool[constant_index as usize];
            },
            _ => panic!("Invalid opcode {}", opcode),
        }
    };
}
