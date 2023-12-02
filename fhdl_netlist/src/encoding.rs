use std::ops::{Deref, DerefMut};

use rustc_serialize::{Decodable, Decoder, Encodable, Encoder};
use smallvec::{Array, SmallVec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Wrap<T>(pub T);

impl<T> Deref for Wrap<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Wrap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> From<T> for Wrap<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<const N: usize, E: Encoder, T: Encodable<E>> Encodable<E> for Wrap<SmallVec<[T; N]>>
where
    [T; N]: Array,
    <[T; N] as Array>::Item: Encodable<E>,
{
    fn encode(&self, s: &mut E) {
        self.0.as_slice().encode(s);
    }
}

impl<const N: usize, D: Decoder, T: Decodable<D>> Decodable<D> for Wrap<SmallVec<[T; N]>>
where
    [T; N]: Array,
    <[T; N] as Array>::Item: Decodable<D>,
{
    fn decode(d: &mut D) -> Self {
        let len = d.read_usize();
        Self((0 .. len).map(|_| Decodable::decode(d)).collect())
    }
}

impl<E: Encoder, T: Encodable<E>> Encodable<E> for Wrap<[T; 3]> {
    fn encode(&self, s: &mut E) {
        self.0.as_slice().encode(s)
    }
}

impl<D: Decoder, T: Decodable<D>> Decodable<D> for Wrap<[T; 3]> {
    fn decode(d: &mut D) -> Self {
        // read the length
        let _ = usize::decode(d);

        Wrap([
            Decodable::decode(d),
            Decodable::decode(d),
            Decodable::decode(d),
        ])
    }
}
