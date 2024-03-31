pub trait IteratorExt: Iterator {
    fn fold_into_one(self) -> Option<Self::Item>
    where
        Self::Item: Clone + PartialEq;
}

impl<I: Iterator> IteratorExt for I {
    fn fold_into_one(self) -> Option<Self::Item>
    where
        Self::Item: Clone + PartialEq,
    {
        enum SingleItem<T> {
            None,
            Single(T),
            Multiple,
        }

        impl<T> SingleItem<T> {
            #[inline]
            fn push(self, item: &T) -> Self
            where
                T: Clone + PartialEq,
            {
                match self {
                    Self::None => Self::Single(item.clone()),
                    Self::Single(old_item) if old_item == *item => Self::Single(old_item),
                    _ => Self::Multiple,
                }
            }

            fn is_multiple(&self) -> bool {
                matches!(self, Self::Multiple)
            }

            fn into_opt(self) -> Option<T> {
                match self {
                    Self::Single(val) => Some(val),
                    _ => None,
                }
            }
        }

        let mut init = SingleItem::None;
        for item in self {
            init = init.push(&item);
            if init.is_multiple() {
                break;
            }
        }

        init.into_opt()
    }
}
