use std::cell::{Cell, RefCell};

pub(crate) trait Iterable<T: 'static> {
    fn next(&self) -> Option<&T>;
    fn peek(&self) -> Option<&T>;
    fn back(&self);
    /// Index of the next item.
    fn pos(&self) -> usize;
}

/// Also tracks the index of the item returned by the latest `next` or `peek`,
/// so that an error can point at the item that caused it.
pub(crate) struct Buffer<T: 'static>(Vec<T>, RefCell<usize>, Cell<usize>);

impl Buffer<char> {
    pub(crate) fn from_string(s: &str) -> Self {
        Self::new(s.chars().collect())
    }
}

impl<T: 'static> Buffer<T> {
    pub(crate) fn new(vec: Vec<T>) -> Self {
        Self(vec, RefCell::new(0), Cell::new(0))
    }

    /// Index of the item returned by the latest `next` or `peek`.
    pub(crate) fn last(&self) -> usize {
        self.2.get()
    }
}

impl<T: 'static> Iterable<T> for Buffer<T> {
    fn next(&self) -> Option<&T> {
        let mut r = self.1.borrow_mut();
        let idx = *r;
        if idx < self.0.len() {
            *r += 1;
        }
        self.2.set(idx);
        self.0.get(idx)
    }

    fn peek(&self) -> Option<&T> {
        let idx = *self.1.borrow();
        self.2.set(idx);
        self.0.get(idx)
    }

    fn back(&self) {
        let mut r = self.1.borrow_mut();
        if *r > 0 {
            *r -= 1;
        }
    }

    fn pos(&self) -> usize {
        *self.1.borrow()
    }
}
