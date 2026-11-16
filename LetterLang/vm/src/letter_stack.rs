use crate::letter_value::LetterValue;
use crate::eval_err::EvaluationErr;
use crate::eval_err::EvaluationErr::*;

const LIMIT: usize = 512;

#[derive(Debug)]
pub struct LetterStack {
    pub buffer: Box<[LetterValue]>,
    pointer: usize,
}

impl LetterStack {
    pub fn new() -> Self {
        LetterStack {
            buffer: vec![LetterValue::None; LIMIT].into_boxed_slice(),
            pointer: 0,
        }
    }

    pub fn push(&mut self, val: LetterValue) -> Result<(), EvaluationErr> {
        if self.pointer == LIMIT { return Err(StackOverflow("push(): StackOverflow")) }

        self.buffer[self.pointer] = val;
        self.pointer += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Result<&LetterValue, EvaluationErr> {
        if self.pointer == 0 { return Err(StackUnderflow("pop(): StackUnderflow")) }

        self.pointer -= 1;
        Ok(&self.buffer[self.pointer])
    }
}
