/// The possible values of a `bool` variable.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoolSet {
    data: u8,
}
impl BoolSet {
    pub const FALSE: Self = BoolSet { data: 1 };
    pub const TRUE: Self = BoolSet { data: 2 };

    /// A bool that is neither `true` nor `false`.
    pub const fn empty() -> Self {
        BoolSet { data: 0 }
    }
    /// A bool that could be `true` or `false`.
    pub const fn all() -> Self {
        BoolSet { data: 3 }
    }
    /// A bool that is `true` or `false`.
    pub const fn constant(value: bool) -> Self {
        BoolSet {
            data: 1 << value as u8,
        }
    }

    pub const fn is_empty(self) -> bool {
        self.data == 0
    }
    pub const fn is_all(self) -> bool {
        self.data == 3
    }
    pub fn contains(self, value: bool) -> bool {
        self.data & (1 << value as u8) != 0
    }

    /// Same as `self = self.union(Self::constant(value))`.
    pub fn insert(&mut self, value: bool) {
        self.data |= Self::constant(value).data;
    }

    pub const fn union(self, other: Self) -> Self {
        BoolSet {
            data: self.data | other.data,
        }
    }
    pub const fn intersection(self, other: Self) -> Self {
        BoolSet {
            data: self.data & other.data,
        }
    }
    pub const fn difference(self, other: Self) -> Self {
        BoolSet {
            data: self.data & !other.data,
        }
    }
}
impl std::ops::BitOr for BoolSet {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        if self.is_empty() || other.is_empty() {
            return BoolSet::empty();
        }

        let mut result = BoolSet::empty();
        if self.contains(true) || other.contains(true) {
            result.insert(true);
        }
        if self.contains(false) && other.contains(false) {
            result.insert(false);
        }
        result
    }
}
impl std::ops::BitAnd for BoolSet {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        if self.is_empty() || other.is_empty() {
            return BoolSet::empty();
        }

        let mut result = BoolSet::empty();
        if self.contains(false) || other.contains(false) {
            result.insert(false);
        }
        if self.contains(true) && other.contains(true) {
            result.insert(true);
        }
        result
    }
}
impl std::ops::BitXor for BoolSet {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        let mut result = BoolSet::empty();
        if self.contains(false) {
            result = result.union(other);
        }
        if self.contains(true) {
            result = result.union(!other);
        }
        result
    }
}
impl std::ops::Not for BoolSet {
    type Output = Self;

    fn not(self) -> Self {
        // swap true and false
        let f = self.data & 1;
        let t = self.data & 2;
        BoolSet {
            data: (t >> 1) | (f << 1),
        }
    }
}

impl std::fmt::Display for BoolSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.contains(true) {
            if self.contains(false) {
                write!(f, "true | false")
            } else {
                write!(f, "true")
            }
        } else if self.contains(false) {
            write!(f, "false")
        } else {
            write!(f, "empty_bool")
        }
    }
}
