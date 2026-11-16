#[derive(Debug, Clone)]
pub enum LetterValue {
    None,
    Number(i32), // TODO: make this a mathematical integer as parsed by the Haskell frontend
}
