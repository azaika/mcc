pub type Span = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub loc: Span
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(item : T, loc : Span) -> Self {
        Self {
            item,
            loc
        }
    }

    #[inline]
    pub fn map<F, U>(self, func : F) -> Spanned<U>
        where F: FnOnce(T) -> U
    {
        Spanned::new(func(self.item), self.loc)
    }
}