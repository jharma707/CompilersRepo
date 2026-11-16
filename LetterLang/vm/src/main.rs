mod evaluator;
mod letter_value;
mod letter_stack;
mod eval_err;

use self::evaluator::*;
use self::letter_stack::LetterStack;
use self::letter_value::LetterValue;
use self::eval_err::EvaluationErr::*;

fn main() {
    let mut evaluator = Evaluator {
        byte_code: vec![OP_CONST, 0, OP_CONST, 1, OP_ADD, OP_HALT], // TODO: turn into a byte code parser
        constant_pool: vec![
            &LetterValue::Number(42),
            &LetterValue::Number(2),
        ],
        stack: LetterStack::new(),
    };

    let value = evaluator.eval();
    match value {
        Ok(LetterValue::Number(i)) => println!("{i}"),
        Ok(LetterValue::None) => println!("None"),
        Err(StackOverflow(msg)) | Err(StackUnderflow(msg)) => panic!("{msg}"),
        Err(NotANumber) => panic!("expected a number"),
    }
}
