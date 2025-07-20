use std::fmt::{Debug, Display};

use rinterval::{IntInterval, IntType};

pub trait Int: Copy + Debug + Display + PartialOrd {
    fn get_type() -> IntType;
    fn to_range(self) -> IntInterval;
    fn range_from(min: Self, max: Self) -> IntInterval;
    fn midpoint(self, other: Self) -> Self;
    fn contains(range: &IntInterval, value: Self) -> bool {
        range.is_superset_of(&value.to_range())
    }
}
macro_rules! impl_signed {
    ($ty:ty, $int_type:expr) => {
        impl Int for $ty {
            fn get_type() -> IntType {
                $int_type
            }
            fn midpoint(self, other: Self) -> Self {
                // mid point is the average of the two values
                self.midpoint(other)
            }
            fn to_range(self) -> IntInterval {
                IntInterval::single_signed($int_type, self as i128)
            }
            fn range_from(min: Self, max: Self) -> IntInterval {
                debug_assert!(min <= max);
                IntInterval::new_signed($int_type, min as i128, max as i128)
            }
        }
    };
}
macro_rules! impl_unsigned {
    ($ty:ty, $int_type:expr) => {
        impl Int for $ty {
            fn get_type() -> IntType {
                $int_type
            }
            fn midpoint(self, other: Self) -> Self {
                // mid point is the average of the two values
                self.midpoint(other)
            }
            fn to_range(self) -> IntInterval {
                IntInterval::single_unsigned($int_type, self as u128)
            }
            fn range_from(min: Self, max: Self) -> IntInterval {
                debug_assert!(min <= max);
                IntInterval::new_unsigned($int_type, min as u128, max as u128)
            }
        }
    };
}
impl_signed!(i8, IntType::I8);
impl_signed!(i16, IntType::I16);
impl_signed!(i32, IntType::I32);
impl_signed!(i64, IntType::I64);
impl_signed!(i128, IntType::I128);
impl_unsigned!(u8, IntType::U8);
impl_unsigned!(u16, IntType::U16);
impl_unsigned!(u32, IntType::U32);
impl_unsigned!(u64, IntType::U64);
impl_unsigned!(u128, IntType::U128);

pub const VALUES_U8: &[u8] = &[
    0,
    1,
    2,
    3,
    4,
    8,
    15,
    16,
    17,
    99,
    100,
    101,
    u8::MAX / 2 - 2,
    u8::MAX / 2 - 1,
    u8::MAX / 2,
    u8::MAX / 2 + 1,
    u8::MAX / 2 + 2,
    u8::MAX - 100,
    u8::MAX - 2,
    u8::MAX - 1,
    u8::MAX,
];
pub const VALUES_I8: &[i8] = &[
    i8::MIN,
    i8::MIN + 1,
    i8::MIN + 2,
    i8::MIN + 100,
    -101,
    -100,
    -10,
    -4,
    -3,
    -2,
    -1,
    0,
    1,
    2,
    3,
    4,
    8,
    15,
    16,
    17,
    99,
    100,
    101,
    i8::MAX / 2 - 2,
    i8::MAX / 2 - 1,
    i8::MAX / 2,
    i8::MAX / 2 + 1,
    i8::MAX / 2 + 2,
    i8::MAX - 100,
    i8::MAX - 2,
    i8::MAX - 1,
    i8::MAX,
];
pub const VALUES_U32: &[u32] = &[
    0,
    1,
    2,
    3,
    8,
    16,
    99,
    100,
    101,
    u32::MAX / 2 - 2,
    u32::MAX / 2 - 1,
    u32::MAX / 2,
    u32::MAX / 2 + 1,
    u32::MAX / 2 + 2,
    u32::MAX - 100,
    u32::MAX - 2,
    u32::MAX - 1,
    u32::MAX,
];
pub const VALUES_I32: &[i32] = &[
    i32::MIN,
    i32::MIN + 1,
    i32::MIN + 2,
    i32::MIN + 100,
    -101,
    -100,
    -10,
    -2,
    -1,
    0,
    1,
    2,
    3,
    8,
    16,
    99,
    100,
    101,
    i32::MAX / 2 - 2,
    i32::MAX / 2 - 1,
    i32::MAX / 2,
    i32::MAX / 2 + 1,
    i32::MAX / 2 + 2,
    i32::MAX - 100,
    i32::MAX - 2,
    i32::MAX - 1,
    i32::MAX,
];
pub const VALUES_U128: &[u128] = &[
    0,
    1,
    2,
    3,
    8,
    16,
    99,
    100,
    101,
    u128::MAX / 2 - 2,
    u128::MAX / 2 - 1,
    u128::MAX / 2,
    u128::MAX / 2 + 1,
    u128::MAX / 2 + 2,
    u128::MAX - 100,
    u128::MAX - 2,
    u128::MAX - 1,
    u128::MAX,
];
pub const VALUES_I128: &[i128] = &[
    i128::MIN,
    i128::MIN + 1,
    i128::MIN + 2,
    i128::MIN + 100,
    -101,
    -100,
    -10,
    -2,
    -1,
    0,
    1,
    2,
    3,
    8,
    16,
    99,
    100,
    101,
    i128::MAX / 2 - 2,
    i128::MAX / 2 - 1,
    i128::MAX / 2,
    i128::MAX / 2 + 1,
    i128::MAX / 2 + 2,
    i128::MAX - 100,
    i128::MAX - 2,
    i128::MAX - 1,
    i128::MAX,
];

macro_rules! single_i {
    ($t:expr, $v:expr) => {
        IntInterval::single_signed($t, $v as i128)
    };
}
macro_rules! single_u {
    ($t:expr, $v:expr) => {
        IntInterval::single_unsigned($t, $v as u128)
    };
}
macro_rules! range_i {
    ($t:expr, $min:expr, $max:expr) => {
        IntInterval::new_signed($t, $min as i128, $max as i128)
    };
}
macro_rules! range_u {
    ($t:expr, $min:expr, $max:expr) => {
        IntInterval::new_unsigned($t, $min as u128, $max as u128)
    };
}

pub const SNAPSHOT_RANGES_U8: &[IntInterval] = &[
    IntInterval::empty(IntType::U8),
    single_u!(IntType::U8, 0),
    single_u!(IntType::U8, 1),
    single_u!(IntType::U8, 2),
    single_u!(IntType::U8, 3),
    single_u!(IntType::U8, 4),
    single_u!(IntType::U8, 10),
    single_u!(IntType::U8, 99),
    single_u!(IntType::U8, 100),
    single_u!(IntType::U8, 101),
    single_u!(IntType::U8, u8::MAX / 2),
    single_u!(IntType::U8, u8::MAX / 2 + 1),
    single_u!(IntType::U8, u8::MAX - 2),
    single_u!(IntType::U8, u8::MAX - 1),
    single_u!(IntType::U8, u8::MAX),
    range_u!(IntType::U8, 0, 255),
    range_u!(IntType::U8, 0, 10),
    range_u!(IntType::U8, 7, 9),
    range_u!(IntType::U8, 4, 8),
    range_u!(IntType::U8, 0, 3),
    range_u!(IntType::U8, 3, 4),
    range_u!(IntType::U8, 1, 100),
    range_u!(IntType::U8, 0, 99),
    range_u!(IntType::U8, 100, 200),
    range_u!(IntType::U8, 1, u8::MAX),
    range_u!(IntType::U8, 0, u8::MAX - 1),
    range_u!(IntType::U8, u8::MAX - 2, u8::MAX),
];
pub const SNAPSHOT_RANGES_U32: &[IntInterval] = &[
    IntInterval::empty(IntType::U32),
    single_u!(IntType::U32, 0),
    single_u!(IntType::U32, 1),
    single_u!(IntType::U32, 2),
    single_u!(IntType::U32, 3),
    single_u!(IntType::U32, 4),
    single_u!(IntType::U32, 10),
    single_u!(IntType::U32, 15),
    single_u!(IntType::U32, 16),
    single_u!(IntType::U32, 99),
    single_u!(IntType::U32, 100),
    single_u!(IntType::U32, 101),
    single_u!(IntType::U32, u32::MAX / 2),
    single_u!(IntType::U32, u32::MAX / 2 + 1),
    single_u!(IntType::U32, u32::MAX - 2),
    single_u!(IntType::U32, u32::MAX - 1),
    single_u!(IntType::U32, u32::MAX),
    range_u!(IntType::U32, 0, u32::MAX),
    range_u!(IntType::U32, 0, 10),
    range_u!(IntType::U32, 7, 9),
    range_u!(IntType::U32, 4, 8),
    range_u!(IntType::U32, 0, 3),
    range_u!(IntType::U32, 3, 4),
    range_u!(IntType::U32, 1, 100),
    range_u!(IntType::U32, 0, 99),
    range_u!(IntType::U32, 1, u32::MAX),
    range_u!(IntType::U32, 0, u32::MAX - 1),
    range_u!(IntType::U32, u32::MAX - 2, u32::MAX),
];

pub const SNAPSHOT_RANGES_I8: &[IntInterval] = &[
    IntInterval::empty(IntType::I8),
    single_i!(IntType::I8, -128),
    single_i!(IntType::I8, -127),
    single_i!(IntType::I8, -126),
    single_i!(IntType::I8, -100),
    single_i!(IntType::I8, -9),
    single_i!(IntType::I8, -2),
    single_i!(IntType::I8, -1),
    single_i!(IntType::I8, 0),
    single_i!(IntType::I8, 1),
    single_i!(IntType::I8, 2),
    single_i!(IntType::I8, 3),
    single_i!(IntType::I8, 4),
    single_i!(IntType::I8, 10),
    single_i!(IntType::I8, 99),
    single_i!(IntType::I8, 100),
    single_i!(IntType::I8, 101),
    single_i!(IntType::I8, 125),
    single_i!(IntType::I8, 126),
    single_i!(IntType::I8, 127),
    range_i!(IntType::I8, i8::MIN, i8::MAX),
    range_i!(IntType::I8, 0, i8::MAX),
    range_i!(IntType::I8, i8::MIN, 0),
    range_i!(IntType::I8, 0, 10),
    range_i!(IntType::I8, 7, 9),
    range_i!(IntType::I8, -5, 3),
    range_i!(IntType::I8, -5, -2),
    range_i!(IntType::I8, -1, 1),
    range_i!(IntType::I8, 1, 100),
    range_i!(IntType::I8, 0, 99),
    range_i!(IntType::I8, i8::MIN + 1, i8::MAX),
    range_i!(IntType::I8, i8::MIN, i8::MAX - 1),
];
pub const SNAPSHOT_RANGES_I32: &[IntInterval] = &[
    IntInterval::empty(IntType::I32),
    single_i!(IntType::I32, i32::MIN),
    single_i!(IntType::I32, i32::MIN + 1),
    single_i!(IntType::I32, i32::MIN + 2),
    single_i!(IntType::I32, -100),
    single_i!(IntType::I32, -9),
    single_i!(IntType::I32, -2),
    single_i!(IntType::I32, -1),
    single_i!(IntType::I32, 0),
    single_i!(IntType::I32, 1),
    single_i!(IntType::I32, 2),
    single_i!(IntType::I32, 3),
    single_i!(IntType::I32, 4),
    single_i!(IntType::I32, 10),
    single_i!(IntType::I32, 99),
    single_i!(IntType::I32, 100),
    single_i!(IntType::I32, 101),
    single_i!(IntType::I32, i32::MAX - 2),
    single_i!(IntType::I32, i32::MAX - 1),
    single_i!(IntType::I32, i32::MAX),
    range_i!(IntType::I32, i32::MIN, i32::MAX),
    range_i!(IntType::I32, 0, i32::MAX),
    range_i!(IntType::I32, i32::MIN, 0),
    range_i!(IntType::I32, 0, 10),
    range_i!(IntType::I32, 7, 9),
    range_i!(IntType::I32, -5, 3),
    range_i!(IntType::I32, -5, -2),
    range_i!(IntType::I32, 1, 100),
    range_i!(IntType::I32, 0, 99),
    range_i!(IntType::I32, i32::MIN + 1, i32::MAX),
    range_i!(IntType::I32, i32::MIN, i32::MAX - 1),
];
