use std::ops::Range;
use std::fmt;

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

impl<T> From<Spanned<T>> for Range<usize> {
    fn from(spanned : Spanned<T>) -> Range<usize> {
        let lo = spanned.loc.0;
        let hi = spanned.loc.1;
        lo..hi
    }
}

impl<T : fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.item)
    }
}