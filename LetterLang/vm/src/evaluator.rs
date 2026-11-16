use crate::letter_value::LetterValue;
use crate::letter_stack::LetterStack;
use crate::eval_err::EvaluationErr;
use crate::eval_err::EvaluationErr::*;

pub const OP_HALT:  u8 = 0;
pub const OP_CONST: u8 = 1;
pub const OP_ADD:   u8 = 2;
pub const OP_SUB:   u8 = 3;
pub const OP_MULT:  u8 = 4;
pub const OP_DIV:   u8 = 5;

pub struct Evaluator<'a> {
    pub byte_code: Vec<u8>,
    pub constant_pool: Vec<&'a LetterValue>,
    pub stack: LetterStack,
}

impl<'a> Evaluator<'a> {
    pub fn eval(&mut self) -> Result<LetterValue, EvaluationErr> {
        if self.byte_code.is_empty() { return Ok(LetterValue::None) }

        // the instruction counter
        let mut ic = 0;

        loop {
            let opcode = self.byte_code[ic]; // TODO: make a READ_BYTE macro
            dbg!(opcode);

            ic = ic + 1;
            match opcode {
                OP_HALT => { return self.stack.pop(); },
                OP_CONST => {
                    let constant_index = self.byte_code[ic];
                    let constant = self.constant_pool[constant_index as usize];

                    self.stack.push(constant)?;
                    ic = ic + 1;
                },
                OP_ADD  => { self.binary_op_number(|op1, op2| op1 + op2)?; }
                OP_SUB  => { self.binary_op_number(|op1, op2| op1 - op2)?; }
                OP_MULT => { self.binary_op_number(|op1, op2| op1 * op2)?; }
                OP_DIV  => { self.binary_op_number(|op1, op2| op1 / op2)?; }
                _ => panic!("Invalid opcode {}", opcode),
            }
        };
    }

    fn binary_op_number<F>(&mut self, f: F) -> Result<LetterValue, EvaluationErr>
        where F: FnOnce(i32, i32) -> i32
    {
        let LetterValue::Number(op2) = self.stack.pop()? else {
            return Err(NotANumber)
        };
        let LetterValue::Number(op1) = self.stack.pop()? else {
            return Err(NotANumber)
        };
        let result = LetterValue::Number(f(op1, op2));
        self.stack.push(&result)?;
        Ok(result)
    }
}
