pub struct Rewind<I: Iterator> {
    iter: I,
    buffer: Vec<I::Item>,
}

#[allow(unused)]
impl<I: Iterator> Rewind<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            buffer: Vec::new(),
        }
    }
    pub fn with_capacity(iter: I, capacity: usize) -> Self {
        Self {
            iter,
            buffer: Vec::with_capacity(capacity),
        }
    }
    pub fn rewind(&mut self, item: I::Item) {
        self.buffer.push(item)
    }
}

impl<I: Iterator> Iterator for Rewind<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.buffer.is_empty() {
            self.iter.next()
        } else {
            self.buffer.pop()
        }
    }
}
