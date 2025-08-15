use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
};

use rinterval::{ArithResult, BoolSet, IInterval, IntType};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoolType;
impl std::fmt::Display for BoolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bool")
    }
}

pub trait InnerType: Eq + Ord + Hash + Debug {}
impl InnerType for IntType {}
impl InnerType for BoolType {}

pub trait WithInnerType: Display + DisplayUntyped {
    type Inner: InnerType;

    fn inner_type(&self) -> Self::Inner;
}
impl WithInnerType for IInterval {
    type Inner = IntType;

    fn inner_type(&self) -> Self::Inner {
        self.ty
    }
}
impl WithInnerType for BoolSet {
    type Inner = BoolType;

    fn inner_type(&self) -> Self::Inner {
        BoolType
    }
}

pub trait DisplayUntyped {
    fn to_string_untyped(&self) -> String;
}
impl DisplayUntyped for IInterval {
    fn to_string_untyped(&self) -> String {
        self.to_string_untyped()
    }
}
impl DisplayUntyped for BoolSet {
    fn to_string_untyped(&self) -> String {
        if self.is_empty() {
            "empty".to_string()
        } else {
            self.to_string()
        }
    }
}

pub trait Int: Copy + Debug + Display + PartialOrd {
    fn int_type() -> IntType;
    fn to_range(self) -> IInterval;
    fn range_from(min: Self, max: Self) -> IInterval;
    fn midpoint(self, other: Self) -> Self;
    fn contains(range: &IInterval, value: Self) -> bool {
        range.is_superset_of(&value.to_range())
    }
}

macro_rules! impl_signed {
    ($ty:ty, $int_type:expr) => {
        impl Int for $ty {
            fn int_type() -> IntType {
                $int_type
            }
            fn midpoint(self, other: Self) -> Self {
                // mid point is the average of the two values
                self.midpoint(other)
            }
            fn to_range(self) -> IInterval {
                IInterval::single_signed($int_type, self as i128)
            }
            fn range_from(min: Self, max: Self) -> IInterval {
                debug_assert!(min <= max);
                IInterval::new_signed($int_type, min as i128, max as i128)
            }
        }
    };
}
macro_rules! impl_unsigned {
    ($ty:ty, $int_type:expr) => {
        impl Int for $ty {
            fn int_type() -> IntType {
                $int_type
            }
            fn midpoint(self, other: Self) -> Self {
                // mid point is the average of the two values
                self.midpoint(other)
            }
            fn to_range(self) -> IInterval {
                IInterval::single_unsigned($int_type, self as u128)
            }
            fn range_from(min: Self, max: Self) -> IInterval {
                debug_assert!(min <= max);
                IInterval::new_unsigned($int_type, min as u128, max as u128)
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

pub trait Output: Copy + Debug + Display {
    type Type: Copy + Eq + Debug;
    type Set: Display;

    fn type_of_self() -> Self::Type;
    fn type_of(set: &Self::Set) -> Self::Type;
    fn contains(range: &Self::Set, value: Self) -> bool;
}

impl<T: Int> Output for T {
    type Type = IntType;
    type Set = IInterval;

    fn type_of_self() -> Self::Type {
        T::int_type()
    }

    fn type_of(set: &Self::Set) -> Self::Type {
        set.ty
    }

    fn contains(range: &Self::Set, value: Self) -> bool {
        T::contains(range, value)
    }
}
impl Output for bool {
    type Type = ();
    type Set = BoolSet;

    fn type_of_self() -> Self::Type {}
    fn type_of(_set: &Self::Set) -> Self::Type {}

    fn contains(range: &Self::Set, value: Self) -> bool {
        range.contains(value)
    }
}

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
    1000,
    1001,
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
    -1000,
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
    1000,
    1001,
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
        IInterval::single_signed($t, $v as i128)
    };
}
macro_rules! single_u {
    ($t:expr, $v:expr) => {
        IInterval::single_unsigned($t, $v as u128)
    };
}
macro_rules! range_i {
    ($t:expr, $min:expr, $max:expr) => {
        IInterval::new_signed($t, $min as i128, $max as i128)
    };
}
macro_rules! range_u {
    ($t:expr, $min:expr, $max:expr) => {
        IInterval::new_unsigned($t, $min as u128, $max as u128)
    };
}

pub const SNAPSHOT_RANGES_U8: &[IInterval] = &[
    IInterval::empty(IntType::U8),
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
pub const SNAPSHOT_RANGES_U32: &[IInterval] = &[
    IInterval::empty(IntType::U32),
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

pub const SNAPSHOT_RANGES_I8: &[IInterval] = &[
    IInterval::empty(IntType::I8),
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
pub const SNAPSHOT_RANGES_I32: &[IInterval] = &[
    IInterval::empty(IntType::I32),
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

fn snapshot_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("snapshots")
}

pub fn match_snapshot(name: &str, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut filename = name
        .to_string()
        .replace(|c: char| !(c.is_alphanumeric() || c == '_'), " ")
        .replace("  ", " ")
        .replace("  ", " ");
    filename = filename
        .strip_prefix("Arithmetic ")
        .unwrap_or(&filename)
        .to_string();

    let snapshot_path = snapshot_dir().join(filename + ".yml");

    if !snapshot_path.exists() {
        std::fs::create_dir_all(snapshot_path.parent().unwrap())?;
        std::fs::write(&snapshot_path, content)?;
    } else {
        let existing_content = std::fs::read_to_string(&snapshot_path)?.replace("\r\n", "\n");
        if existing_content != content {
            std::fs::write(&snapshot_path, content)?;
            return Err("Snapshot mismatch".into());
        }
    }
    Ok(())
}

pub fn assert_snapshot(name: &str, content: &str) {
    if let Err(e) = match_snapshot(name, content) {
        panic!("Snapshot assertion failed for {name}: {e}");
    }
}

fn get_types<'a, T: WithInnerType + 'static>(
    ranges: impl Iterator<Item = &'a T>,
) -> (usize, String) {
    let unique_types: HashSet<_> = ranges.map(|r| r.inner_type()).collect();
    if unique_types.is_empty() {
        return (0, "none".to_string());
    }
    let mut unique_types = Vec::from_iter(unique_types);
    unique_types.sort();

    let formattted = unique_types
        .iter()
        .map(|ty| format!("{ty:?}"))
        .collect::<Vec<_>>()
        .join(" | ");

    (unique_types.len(), formattted)
}
pub fn format_unary_data<R: WithInnerType + 'static>(
    data: &[(IInterval, ArithResult<R>)],
) -> String {
    let input_types = get_types(data.iter().map(|(i, _)| i));
    let mut result_types = get_types(data.iter().filter_map(|(_, r)| r.as_ref().ok()));
    let has_error = data.iter().any(|(_, res)| res.is_err());
    if has_error {
        result_types.1 += " | Error";
    }

    let mut out = String::new();
    out.push_str(&format!("{} => {}: >\n", input_types.1, result_types.1));

    for (input, res) in data {
        out.push_str("  "); // indent

        if input_types.0 == 1 {
            out.push_str(&input.to_string_untyped());
        } else {
            out.push_str(&input.to_string());
        }

        out.push_str("  ->  ");

        let range = match res {
            Ok(range) => {
                if result_types.0 == 1 {
                    range.to_string_untyped()
                } else {
                    range.to_string()
                }
            }
            Err(err) => format!("Error: {err:?}"),
        };
        out.push_str(&range);
        out.push('\n');
    }

    out
}
pub fn format_binary_data<R: WithInnerType + 'static>(
    data: &[(IInterval, IInterval, ArithResult<R>)],
) -> String {
    let lhs_types = get_types(data.iter().map(|(lhs, _, _)| lhs));
    let rhs_types = get_types(data.iter().map(|(_, rhs, _)| rhs));
    let mut result_types = get_types(data.iter().filter_map(|(_, _, r)| r.as_ref().ok()));
    let has_error = data.iter().any(|(_, _, res)| res.is_err());
    if has_error {
        result_types.1 += " | Error";
    }

    let mut out = String::new();
    out.push_str(&format!(
        "{} x {} => {}: >\n",
        lhs_types.1, rhs_types.1, result_types.1
    ));

    for (lhs, rhs, res) in data {
        out.push_str("  "); // indent

        if lhs_types.0 == 1 && rhs_types.0 == 1 {
            out.push_str(&format!(
                "{}  .  {}",
                lhs.to_string_untyped(),
                rhs.to_string_untyped()
            ));
        } else {
            out.push_str(&format!("{lhs}  .  {rhs}"));
        }

        out.push_str("  ->  ");

        let range = match res {
            Ok(range) => {
                if result_types.0 == 1 {
                    range.to_string_untyped()
                } else {
                    range.to_string()
                }
            }
            Err(err) => format!("Error: {err:?}"),
        };
        out.push_str(&range);
        out.push('\n');
    }

    out
}
