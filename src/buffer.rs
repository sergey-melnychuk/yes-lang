use std::cell::RefCell;

pub(crate) trait Iterable<T: 'static> {
    fn next(&self) -> Option<&T>;
    fn peek(&self) -> Option<&T>;
}

pub(crate) struct Buffer<T: 'static>(Vec<T>, RefCell<usize>);

impl Buffer<char> {
    pub(crate) fn from_string(s: &str) -> Self {
        Self(s.chars().collect(), RefCell::new(0))
    }
}

impl<T: 'static> Buffer<T> {
    pub(crate) fn new(vec: Vec<T>) -> Self {
        Self(vec, RefCell::new(0))
    }
}


impl<T: 'static> Iterable<T> for Buffer<T> {
    fn next(&self) -> Option<&T> {
        let mut r = self.1.borrow_mut();
        let idx = *r;
        if idx < self.0.len() {
            *r += 1;
        }
        self.0.iter().skip(idx).next()
    }

    fn peek(&self) -> Option<&T> {
        self.0.iter().skip(*self.1.borrow()).next()
    }
}
