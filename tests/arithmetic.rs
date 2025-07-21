use rinterval::{ArithResult, Arithmetic, IInterval};

mod data;
mod snapshot;

use data::*;
use snapshot::*;

fn for_each_range<I: Int>(values: &[I], mut f: impl FnMut(IInterval, &[I])) {
    f(IInterval::empty(I::get_type()), &[]);

    for &v in values {
        f(v.to_range(), &[v]);
    }

    for &min in values {
        for &max in values {
            if min > max {
                continue; // duplicates
            }

            f(I::range_from(min, max), &[min, min.midpoint(max), max]);
        }
    }
}

fn test_unary<A: Int, R: Int>(
    values: &[A],
    scalar_op: impl Fn(A) -> Option<R>,
    range_op: fn(&IInterval) -> ArithResult,
) {
    for_each_range(values, |input_range, samples| {
        let range = match range_op(&input_range) {
            Ok(range) => range,
            Err(err) => panic!("Unexpected error: {err:?} for op({input_range})"),
        };
        assert_eq!(
            range.ty,
            R::get_type(),
            "Invalid type for op({input_range}) => {range}",
        );

        for &sample in samples {
            let scalar = scalar_op(sample);
            if let Some(scalar) = scalar {
                assert!(
                    Int::contains(&range, scalar),
                    "Expected op({input_range}) => {range} to contain scalar value op({sample}) => {scalar}",
                );
            }
        }
    });
}
fn test_binary<A: Int, B: Int, R: Int>(
    lhs_values: &[A],
    rhs_values: &[B],
    scalar_op: impl Fn(A, B) -> Option<R>,
    range_op: fn(&IInterval, &IInterval) -> ArithResult,
) {
    for_each_range(lhs_values, |lhs_range, lhs_samples| {
        for_each_range(rhs_values, |rhs_range, rhs_samples| {
            let range = match range_op(&lhs_range, &rhs_range) {
                Ok(range) => range,
                Err(err) => panic!("Unexpected error: {err:?} for op({lhs_range}, {rhs_range})"),
            };

            assert_eq!(
                range.ty,
                R::get_type(),
                "Invalid type for op({lhs_range}, {rhs_range}) => {range}",
            );

            for &lhs in lhs_samples {
                for &rhs in rhs_samples {
                    let scalar = scalar_op(lhs, rhs);
                    if let Some(scalar) = scalar {
                        assert!(
                            Int::contains(&range, scalar),
                            "Expected op({lhs_range}, {rhs_range}) => {range} to contain scalar value op({lhs}, {rhs}) => {scalar}",
                        );
                    }
                }
            }
        });
    });
}

macro_rules! test_unary_signed {
    ($op:expr, $f:expr) => {
        test_unary(VALUES_I32, $f, ($op).range_op);
        test_unary(VALUES_I128, $f, ($op).range_op);
    };
}
macro_rules! test_unary_unsigned {
    ($op:expr, $f:expr) => {
        test_unary(VALUES_U32, $f, ($op).range_op);
        test_unary(VALUES_U128, $f, ($op).range_op);
    };
}
macro_rules! test_unary_all {
    ($op:expr, $f:expr) => {
        test_unary_signed!($op, $f);
        test_unary_unsigned!($op, $f);
    };
}

macro_rules! test_binary_signed {
    ($op:expr, $f:expr) => {
        test_binary(VALUES_I8, VALUES_I8, $f, ($op).range_op);
        test_binary(VALUES_I32, VALUES_I32, $f, ($op).range_op);
        test_binary(VALUES_I128, VALUES_I128, $f, ($op).range_op);
    };
}
macro_rules! test_binary_unsigned {
    ($op:expr, $f:expr) => {
        test_binary(VALUES_U8, VALUES_U8, $f, ($op).range_op);
        test_binary(VALUES_U32, VALUES_U32, $f, ($op).range_op);
        test_binary(VALUES_U128, VALUES_U128, $f, ($op).range_op);
    };
}
macro_rules! test_binary_all {
    ($op:expr, $f:expr) => {
        test_binary_signed!($op, $f);
        test_binary_unsigned!($op, $f);
    };
}

struct UnaryOp {
    name: &'static str,
    range_op: fn(&IInterval) -> ArithResult,
    support_signed: bool,
    support_unsigned: bool,
}
impl UnaryOp {
    fn new(name: &'static str, range_op: fn(&IInterval) -> ArithResult) -> Self {
        Self {
            name,
            range_op,
            support_signed: true,
            support_unsigned: true,
        }
    }

    fn only_signed(mut self) -> Self {
        self.support_unsigned = false;
        self
    }
    fn only_unsigned(mut self) -> Self {
        self.support_signed = false;
        self
    }

    fn snapshot(&self) {
        let mut data = Vec::new();
        if self.support_signed {
            data.push(SNAPSHOT_RANGES_I8);
            data.push(SNAPSHOT_RANGES_I32);
        }
        if self.support_unsigned {
            data.push(SNAPSHOT_RANGES_U8);
            data.push(SNAPSHOT_RANGES_U32);
        }

        self.snapshot_with(&data);
    }
    fn snapshot_with(&self, inputs: &[&[IInterval]]) {
        let mut snapshot = format!("# {}\n\n", self.name);

        for ranges in inputs {
            let mut data = Vec::new();

            for range in *ranges {
                let res = (self.range_op)(range);
                data.push((range.clone(), res));
            }

            snapshot.push_str(&format_unary_data(&data));
            snapshot.push('\n');
        }

        assert_snapshot(self.name, &snapshot);
    }
}

struct BinaryOp {
    name: &'static str,
    range_op: fn(&IInterval, &IInterval) -> ArithResult,
    support_signed: bool,
    support_unsigned: bool,
    commutative: bool,
}
impl BinaryOp {
    fn new(name: &'static str, range_op: fn(&IInterval, &IInterval) -> ArithResult) -> Self {
        Self {
            name,
            range_op,
            support_signed: true,
            support_unsigned: true,
            commutative: false,
        }
    }

    fn only_signed(mut self) -> Self {
        self.support_unsigned = false;
        self
    }
    fn only_unsigned(mut self) -> Self {
        self.support_signed = false;
        self
    }

    fn commutative(mut self) -> Self {
        self.commutative = true;
        self
    }

    fn snapshot(&self) {
        let mut data = Vec::new();
        if self.support_signed {
            data.push((SNAPSHOT_RANGES_I8, SNAPSHOT_RANGES_I8));
            data.push((SNAPSHOT_RANGES_I32, SNAPSHOT_RANGES_I32));
        }
        if self.support_unsigned {
            data.push((SNAPSHOT_RANGES_U8, SNAPSHOT_RANGES_U8));
            data.push((SNAPSHOT_RANGES_U32, SNAPSHOT_RANGES_U32));
        }

        self.snapshot_with(&data);
    }
    fn snapshot_with(&self, inputs: &[(&[IInterval], &[IInterval])]) {
        let mut snapshot = format!("# {}\n\n", self.name);

        for &(lhs_ranges, rhs_ranges) in inputs {
            let mut data = Vec::new();

            for (l_index, lhs_range) in lhs_ranges.iter().enumerate() {
                for (r_index, rhs_range) in rhs_ranges.iter().enumerate() {
                    if self.commutative && l_index > r_index {
                        continue; // skip duplicates
                    }

                    let res = (self.range_op)(lhs_range, rhs_range);
                    data.push((lhs_range.clone(), rhs_range.clone(), res));
                }
            }

            snapshot.push_str(&format_binary_data(&data));
            snapshot.push('\n');
        }

        assert_snapshot(self.name, &snapshot);
    }
}

macro_rules! unary {
    ($f:expr) => {
        UnaryOp::new(stringify!($f), $f)
    };
}
macro_rules! binary {
    ($f:expr) => {
        BinaryOp::new(stringify!($f), $f)
    };
}

#[test]
fn test_saturating_add() {
    let op = binary!(Arithmetic::saturating_add).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.saturating_add(rhs)));
    op.snapshot();
}
#[test]
fn test_strict_add() {
    let op = binary!(Arithmetic::strict_add).commutative();
    test_binary_all!(op, |lhs, rhs| lhs.checked_add(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_add() {
    let op = binary!(Arithmetic::wrapping_add).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.wrapping_add(rhs)));
    op.snapshot();
}

#[test]
fn test_saturating_neg() {
    let op = unary!(Arithmetic::saturating_neg).only_signed();
    test_unary_signed!(op, |x| Some(x.saturating_neg()));
    op.snapshot();
}
#[test]
fn test_strict_neg() {
    let op = unary!(Arithmetic::strict_neg);
    test_unary_all!(op, |x| x.checked_neg());
    op.snapshot();
}
#[test]
fn test_wrapping_neg() {
    let op = unary!(Arithmetic::wrapping_neg);
    test_unary_all!(op, |x| Some(x.wrapping_neg()));
    op.snapshot();
}

#[test]
fn test_saturating_sub() {
    let op = binary!(Arithmetic::saturating_sub);
    test_binary_all!(op, |lhs, rhs| Some(lhs.saturating_sub(rhs)));
    op.snapshot();
}
#[test]
fn test_strict_sub() {
    let op = binary!(Arithmetic::strict_sub);
    test_binary_all!(op, |lhs, rhs| lhs.checked_sub(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_sub() {
    let op = binary!(Arithmetic::wrapping_sub);
    test_binary_all!(op, |lhs, rhs| Some(lhs.wrapping_sub(rhs)));
    op.snapshot();
}

#[test]
fn test_saturating_mul() {
    let op = binary!(Arithmetic::saturating_mul).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.saturating_mul(rhs)));
    op.snapshot();
}
#[test]
fn test_strict_mul() {
    let op = binary!(Arithmetic::strict_mul).commutative();
    test_binary_all!(op, |lhs, rhs| lhs.checked_mul(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_mul() {
    let op = binary!(Arithmetic::wrapping_mul).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.wrapping_mul(rhs)));
    op.snapshot();
}

#[test]
fn test_saturating_div() {
    let op = binary!(Arithmetic::saturating_div);
    test_binary_all!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.saturating_div(rhs))
    });
    op.snapshot();
}
#[test]
fn test_strict_div() {
    let op = binary!(Arithmetic::strict_div);
    test_binary_all!(op, |lhs, rhs| lhs.checked_div(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_div() {
    let op = binary!(Arithmetic::wrapping_div);
    test_binary_all!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.wrapping_div(rhs))
    });
    op.snapshot();
}

#[test]
fn test_strict_rem() {
    let op = binary!(Arithmetic::strict_rem);
    test_binary_all!(op, |lhs, rhs| lhs.checked_rem(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_rem() {
    let op = binary!(Arithmetic::wrapping_rem);
    test_binary_all!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.wrapping_rem(rhs))
    });
    op.snapshot();
}

#[test]
fn test_saturating_abs() {
    let op = unary!(Arithmetic::saturating_abs).only_signed();
    test_unary_signed!(op, |x| Some(x.saturating_abs()));
    op.snapshot();
}
#[test]
fn test_strict_abs() {
    let op = unary!(Arithmetic::strict_abs).only_signed();
    test_unary_signed!(op, |x| x.checked_abs());
    op.snapshot();
}
#[test]
fn test_wrapping_abs() {
    let op = unary!(Arithmetic::wrapping_abs).only_signed();
    test_unary_signed!(op, |x| Some(x.wrapping_abs()));
    op.snapshot();
}

#[test]
fn test_min() {
    let op = binary!(Arithmetic::min).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.min(rhs)));
    op.snapshot();
}
#[test]
fn test_max() {
    let op = binary!(Arithmetic::max).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.max(rhs)));
    op.snapshot();
}

#[test]
fn test_and() {
    let op = binary!(Arithmetic::and).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs & rhs));
    op.snapshot();
}
#[test]
fn test_or() {
    let op = binary!(Arithmetic::or).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs | rhs));
    op.snapshot();
}
#[test]
fn test_xor() {
    let op = binary!(Arithmetic::xor).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs ^ rhs));
    op.snapshot();
}
#[test]
fn test_not() {
    let op = unary!(Arithmetic::not);
    test_unary_all!(op, |x| Some(!x));
    op.snapshot();
}

#[test]
fn test_cast_signed() {
    let op = unary!(Arithmetic::cast_signed).only_unsigned();
    test_unary_unsigned!(op, |x| Some(x.cast_signed()));
    op.snapshot();
}
#[test]
fn test_cast_unsigned() {
    let op = unary!(Arithmetic::cast_unsigned).only_signed();
    test_unary_signed!(op, |x| Some(x.cast_unsigned()));
    op.snapshot();
}

#[test]
fn test_leading_ones() {
    let op = unary!(Arithmetic::leading_ones);
    test_unary_all!(op, |x| Some(x.leading_ones()));
    op.snapshot();
}
#[test]
fn test_leading_zeros() {
    let op = unary!(Arithmetic::leading_zeros);
    test_unary_all!(op, |x| Some(x.leading_zeros()));
    op.snapshot();
}
#[test]
fn test_trailing_ones() {
    let op = unary!(Arithmetic::trailing_ones);
    test_unary_all!(op, |x| Some(x.trailing_ones()));
    op.snapshot();
}
#[test]
fn test_trailing_zeros() {
    let op = unary!(Arithmetic::trailing_zeros);
    test_unary_all!(op, |x| Some(x.trailing_zeros()));
    op.snapshot();
}
#[test]
fn test_count_ones() {
    let op = unary!(Arithmetic::count_ones);
    test_unary_all!(op, |x| Some(x.count_ones()));
    op.snapshot();
}
#[test]
fn test_count_zeros() {
    let op = unary!(Arithmetic::count_zeros);
    test_unary_all!(op, |x| Some(x.count_zeros()));
    op.snapshot();
}
