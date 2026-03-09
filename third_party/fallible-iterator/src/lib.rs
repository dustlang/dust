#![cfg_attr(not(feature = "std"), no_std)]

use core::marker::PhantomData;

pub trait FallibleIterator {
    type Item;
    type Error;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

pub struct Empty<T, E>(PhantomData<(T, E)>);

impl<T, E> Empty<T, E> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T, E> Default for Empty<T, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, E> FallibleIterator for Empty<T, E> {
    type Item = T;
    type Error = E;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        Ok(None)
    }
}
