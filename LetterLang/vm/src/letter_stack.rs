use crate::letter_value::LetterValue;
use crate::letter_value::LetterValue::*;

const LIMIT: usize = 100;

pub struct LetterStack<'a> {
    buffer: &'a mut [LetterValue],
    pointer: usize,
}

impl<'a> LetterStack<'a> {
    pub fn new() -> Self {
        LetterStack {
            buffer: &mut vec![Number(0); LIMIT].into_boxed_slice(),
            pointer: 0,
        }
    }
}
