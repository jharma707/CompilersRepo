use crate::letter_value::LetterValue;

const LIMIT: usize = 512;

pub struct LetterStack<'a> {
    buffer: Box<[&'a LetterValue]>,
    pointer: usize,
}

#[derive(Debug)]
pub struct StackOverflow(pub &'static str);
#[derive(Debug)]
pub struct StackUnderflow(pub &'static str);

impl<'a> LetterStack<'a> {
    pub fn new() -> Self {
        LetterStack {
            buffer: vec![&LetterValue::None; LIMIT].into_boxed_slice(),
            pointer: 0,
        }
    }

    pub fn push(&mut self, val: &'a LetterValue) -> Result<(), StackOverflow> {
        if self.pointer == LIMIT { return Err(StackOverflow("push(): StackOverflow")) }

        self.buffer[self.pointer] = val;
        self.pointer += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Result<&'a LetterValue, StackUnderflow> {
        if self.pointer == 0 { return Err(StackUnderflow("pop(): StackUnderflow")) }

        self.pointer -= 1;
        Ok(self.buffer[self.pointer])
    }
}
