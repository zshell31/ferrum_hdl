pub trait IteratorExt: Iterator {
    fn fold_into_one(self) -> Option<Self::Item>
    where
        Self::Item: Clone + PartialEq;
}

#[derive(Debug, Default, Clone)]
pub enum SingleItem<T> {
    #[default]
    None,
    Single(T),
    Multiple,
}

impl<T> SingleItem<T> {
    #[inline]
    pub fn push(self, item: T) -> Self
    where
        T: PartialEq,
    {
        match self {
            Self::None => Self::Single(item),
            Self::Single(old_item) if old_item == item => Self::Single(old_item),
            _ => Self::Multiple,
        }
    }

    pub fn is_multiple(&self) -> bool {
        matches!(self, Self::Multiple)
    }

    pub fn into_opt(self) -> Option<T> {
        match self {
            Self::Single(val) => Some(val),
            _ => None,
        }
    }
}

impl<I: Iterator> IteratorExt for I {
    fn fold_into_one(self) -> Option<Self::Item>
    where
        Self::Item: PartialEq,
    {
        let mut init = SingleItem::None;
        for item in self {
            init = init.push(item);
            if init.is_multiple() {
                break;
            }
        }

        init.into_opt()
    }
}
