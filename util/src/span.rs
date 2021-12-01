pub type Span = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub loc: Span
}

impl<T> Spanned<T> {
    pub fn new(item : T, loc : Span) -> Self {
        Self {
            item,
            loc
        }
    }
}