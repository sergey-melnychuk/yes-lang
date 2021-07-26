pub(crate) trait Iterable<T: 'static> {
    fn next(&mut self) -> Option<&T>;
    fn peek(&self) -> Option<&T>;
}

pub(crate) struct Buffer<T: 'static>(Vec<T>, usize);

impl Buffer<char> {
    pub(crate) fn new(s: &str) -> Self {
        Self(s.chars().collect(), 0)
    }
}

impl<T: 'static> Iterable<T> for Buffer<T> {
    fn next(&mut self) -> Option<&T> {
        let idx = self.1;
        if idx < self.0.len() {
            self.1 += 1;
        }
        self.0.iter().skip(idx).next()
    }

    fn peek(&self) -> Option<&T> {
        self.0.iter().skip(self.1).next()
    }
}
