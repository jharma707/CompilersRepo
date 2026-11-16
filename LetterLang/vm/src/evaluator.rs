use crate::letter_value::LetterValue;
use crate::letter_stack::LetterStack;
use crate::letter_stack::StackOverflow;

pub const OP_HALT: u8 = 0;
pub const OP_CONST: u8 = 1;

pub struct Evaluator<'a> {
    pub byte_code: Vec<u8>,
    pub constant_pool: Vec<&'a LetterValue>,
    pub stack: LetterStack<'a>,
}

impl<'a> Evaluator<'a> {
    pub fn eval(&mut self) -> &'a LetterValue {
        if self.byte_code.is_empty() { return &LetterValue::None }

        // the instruction counter
        let mut ic = 0;

        loop {
            let opcode = self.byte_code[ic]; // TODO: make a READ_BYTE macro
            dbg!(opcode);

            ic = ic + 1;
            match opcode {
                OP_HALT => {
                    return self.stack.pop()
                        .expect("Halt instruction should be called with a value on the VM stack.")
                },
                OP_CONST => {
                    let constant_index = self.byte_code[ic];
                    let constant = self.constant_pool[constant_index as usize];

                    match self.stack.push(constant) {
                        Err(StackOverflow(msg)) => panic!("{msg}"),
                        Ok(_) => {},
                    }

                    ic = ic + 1;
                },
                _ => panic!("Invalid opcode {}", opcode),
            }
        };
    }
}
