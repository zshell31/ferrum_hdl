use std::{
    fmt,
    fmt::Display,
    ops::{Deref, DerefMut},
};

use darling::FromMeta;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NonEmptyStr(pub String);

impl Display for NonEmptyStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for NonEmptyStr {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NonEmptyStr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromMeta for NonEmptyStr {
    fn from_string(value: &str) -> darling::Result<Self> {
        let value = value.trim();
        if !value.is_empty() {
            Ok(Self(value.to_string()))
        } else {
            Err(darling::Error::custom("empty string"))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NonEmptyAsciiStr(pub String);

impl Display for NonEmptyAsciiStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for NonEmptyAsciiStr {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NonEmptyAsciiStr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromMeta for NonEmptyAsciiStr {
    fn from_string(value: &str) -> darling::Result<Self> {
        let value = value.trim();
        if !value.is_empty() && value.chars().all(|ch| ch.is_ascii_alphanumeric()) {
            Ok(Self(value.to_string()))
        } else {
            Err(darling::Error::custom("empty or non-alphanumeric string"))
        }
    }
}
