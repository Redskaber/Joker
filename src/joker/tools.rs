//! This file is joker tools rs
//!
//!
//!

use std::{cmp::Ordering, hash::Hash};

use super::{
    error::JokerError,
    token::{FromToken, Token},
};

#[derive(Debug, Clone)]
pub struct Key(pub Token);
impl Key {
    pub fn token(&self) -> &Token {
        &self.0
    }
}
impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&self.0.ttype, &self.0.lexeme, &self.0.literal).hash(state)
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.0.ttype == other.0.ttype
            && self.0.lexeme == other.0.lexeme
            && self.0.literal == other.0.literal
    }
}

impl Eq for Key {}

impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl FromToken for Key {
    type Err = JokerError;
    fn from_token(token: &Token) -> Result<Self, Self::Err> {
        Ok(Key(token.clone()))
    }
}
