use std::iter;

pub trait InOut<T: 'static> {
    fn items_len(&self) -> usize;

    fn items(&self) -> impl Iterator<Item = (usize, &T)>;

    fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)>;

    fn by_ind(&self, ind: usize) -> &T;

    fn by_ind_mut(&mut self, ind: usize) -> &mut T;
}

impl<T: 'static> InOut<T> for T {
    fn items_len(&self) -> usize {
        1
    }

    fn items(&self) -> impl Iterator<Item = (usize, &T)> {
        iter::once(self).enumerate()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        iter::once(self).enumerate()
    }

    fn by_ind(&self, ind: usize) -> &T {
        [self][ind]
    }

    fn by_ind_mut(&mut self, ind: usize) -> &mut T {
        [self][ind]
    }
}

impl<T: 'static> InOut<T> for [T] {
    fn items_len(&self) -> usize {
        self.len()
    }

    fn items(&self) -> impl Iterator<Item = (usize, &T)> {
        self.iter().enumerate()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.iter_mut().enumerate()
    }

    fn by_ind(&self, ind: usize) -> &T {
        &self[ind]
    }

    fn by_ind_mut(&mut self, ind: usize) -> &mut T {
        &mut self[ind]
    }
}

impl<T: 'static> InOut<T> for (T, T) {
    fn items_len(&self) -> usize {
        2
    }

    fn items(&self) -> impl Iterator<Item = (usize, &T)> {
        [&self.0, &self.1].into_iter().enumerate()
    }

    fn items_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        [&mut self.0, &mut self.1].into_iter().enumerate()
    }

    fn by_ind(&self, ind: usize) -> &T {
        [&self.0, &self.1][ind]
    }

    fn by_ind_mut(&mut self, ind: usize) -> &mut T {
        [&mut self.0, &mut self.1][ind]
    }
}
