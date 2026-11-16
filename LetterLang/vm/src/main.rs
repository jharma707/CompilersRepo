mod evaluator;
mod letter_value;
mod letter_stack;

use self::evaluator::*;
use self::letter_stack::LetterStack;
use self::letter_value::LetterValue;

fn main() {
    let mut evaluator = Evaluator {
        byte_code: vec![OP_CONST, 0, OP_HALT], // TODO: turn into a byte code parser
        constant_pool: vec![&LetterValue::Number(42)],
        stack: LetterStack::new(),
    };

    let value = evaluator.eval();
    match value {
        LetterValue::Number(i) => println!("{i}"),
        LetterValue::None => println!("None"),
    }
}
