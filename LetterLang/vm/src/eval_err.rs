#[derive(Debug)]
pub enum EvaluationErr {
    StackOverflow(&'static str),
    StackUnderflow(&'static str),
    NotANumber,
}
