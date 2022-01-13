use std::fmt;
use std::hash::Hash;
use std::ops::Range;

pub type Span = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub loc: Span,
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(item: T, loc: Span) -> Self {
        Self { item, loc }
    }

    #[inline]
    pub fn map<F, U>(self, func: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned::new(func(self.item), self.loc)
    }
}

impl<T> From<Spanned<T>> for Range<usize> {
    fn from(spanned: Spanned<T>) -> Range<usize> {
        let lo = spanned.loc.0;
        let hi = spanned.loc.1;
        lo..hi
    }
}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.hash(state);
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.item)
    }
}

pub trait ToSpanned
where
    Self: Sized,
{
    fn with_span(self, span: Span) -> Spanned<Self>;
}

impl<T> ToSpanned for T {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }
}
