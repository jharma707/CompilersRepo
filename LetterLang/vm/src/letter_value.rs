#[derive(Debug, Clone, Default)]
pub enum LetterValue {
    #[default]
    None,
    Number(i32), // TODO: make this a mathematical integer as parsed by the Haskell frontend
}
