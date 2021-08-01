use std::cell::RefCell;

pub(crate) trait Iterable<T: 'static> {
    fn next(&self) -> Option<&T>;
    fn peek(&self) -> Option<&T>;
    fn back(&self);
}

pub(crate) struct Buffer<T: 'static>(Vec<T>, RefCell<usize>);

pub(crate) struct Pos(usize);

impl Buffer<char> {
    pub(crate) fn from_string(s: &str) -> Self {
        Self(s.chars().collect(), RefCell::new(0))
    }
}

impl<T: 'static> Buffer<T> {
    pub(crate) fn new(vec: Vec<T>) -> Self {
        Self(vec, RefCell::new(0))
    }

    pub(crate) fn pos(&self) -> Pos {
        Pos(*self.1.borrow())
    }

    pub(crate) fn set(&self, pos: Pos) {
        *self.1.borrow_mut() = pos.0;
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

    fn back(&self) {
        let mut r = self.1.borrow_mut();
        if *r > 0 {
            *r -= 1;
        }
    }
}
