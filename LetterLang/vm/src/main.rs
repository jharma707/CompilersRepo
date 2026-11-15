mod evaluator;
mod letter_value;
mod letter_stack;

use self::evaluator::*;
use self::letter_value::LetterStack;
use self::letter_value::LetterValue;
use self::letter_value::LetterValue::*;

fn main() {
    let byte_code = vec![OP_HALT]; // TODO: turn into a byte code parser
    let constant_pool = vec![Number(42)];
    let stack = LetterStack::new();

    eval(&byte_code, &constant_pool, &mut stack);
}
