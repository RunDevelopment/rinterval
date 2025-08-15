use rinterval::*;

mod util;

use util::*;

fn for_each_range<I: Int>(values: &[I], mut f: impl FnMut(IInterval, &[I])) {
    f(IInterval::empty(I::int_type()), &[]);

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

fn test_unary<A: Int, R: Output>(
    values: &[A],
    scalar_op: impl Fn(A) -> Option<R>,
    range_op: fn(&IInterval) -> ArithResult<R::Set>,
) {
    for_each_range(values, |input_range, samples| {
        let range = match range_op(&input_range) {
            Ok(range) => range,
            Err(err) => panic!("Unexpected error: {err:?} for op({input_range})"),
        };
        assert_eq!(
            R::type_of(&range),
            R::type_of_self(),
            "Invalid type for op({input_range}) => {range}",
        );

        for &sample in samples {
            let scalar = scalar_op(sample);
            if let Some(scalar) = scalar {
                assert!(
                    R::contains(&range, scalar),
                    "Expected op({input_range}) => {range} to contain scalar value op({sample}) => {scalar}",
                );
            }
        }
    });
}
fn test_binary<A: Int, B: Int, R: Output>(
    lhs_values: &[A],
    rhs_values: &[B],
    scalar_op: impl Fn(A, B) -> Option<R>,
    range_op: fn(&IInterval, &IInterval) -> ArithResult<R::Set>,
) {
    for_each_range(lhs_values, |lhs_range, lhs_samples| {
        for_each_range(rhs_values, |rhs_range, rhs_samples| {
            let range = match range_op(&lhs_range, &rhs_range) {
                Ok(range) => range,
                Err(err) => panic!("Unexpected error: {err:?} for op({lhs_range}, {rhs_range})"),
            };

            assert_eq!(
                R::type_of(&range),
                R::type_of_self(),
                "Invalid type for op({lhs_range}, {rhs_range}) => {range}",
            );

            for &lhs in lhs_samples {
                for &rhs in rhs_samples {
                    let scalar = scalar_op(lhs, rhs);
                    if let Some(scalar) = scalar {
                        assert!(
                            R::contains(&range, scalar),
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
    ($op:expr, $f:expr, rhs = $r:ident) => {
        test_binary(VALUES_I8, $r, $f, ($op).range_op);
        test_binary(VALUES_I32, $r, $f, ($op).range_op);
        test_binary(VALUES_I128, $r, $f, ($op).range_op);
    };
}
macro_rules! test_binary_unsigned {
    ($op:expr, $f:expr) => {
        test_binary(VALUES_U8, VALUES_U8, $f, ($op).range_op);
        test_binary(VALUES_U32, VALUES_U32, $f, ($op).range_op);
        test_binary(VALUES_U128, VALUES_U128, $f, ($op).range_op);
    };
    ($op:expr, $f:expr, rhs = $r:ident) => {
        test_binary(VALUES_U8, $r, $f, ($op).range_op);
        test_binary(VALUES_U32, $r, $f, ($op).range_op);
        test_binary(VALUES_U128, $r, $f, ($op).range_op);
    };
}
macro_rules! test_binary_all {
    ($op:expr, $f:expr) => {
        test_binary_signed!($op, $f);
        test_binary_unsigned!($op, $f);
    };
    ($op:expr, $f:expr, rhs = $r:ident) => {
        test_binary_signed!($op, $f, rhs = $r);
        test_binary_unsigned!($op, $f, rhs = $r);
    };
}

struct UnaryOp<R> {
    name: &'static str,
    range_op: fn(&IInterval) -> ArithResult<R>,
    support_signed: bool,
    support_unsigned: bool,
}
impl<R: 'static> UnaryOp<R> {
    fn new(name: &'static str, range_op: fn(&IInterval) -> ArithResult<R>) -> Self {
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

    fn snapshot(&self)
    where
        R: WithInnerType,
    {
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
    fn snapshot_with(&self, inputs: &[&[IInterval]])
    where
        R: WithInnerType,
    {
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

struct BinaryOp<R> {
    name: &'static str,
    range_op: fn(&IInterval, &IInterval) -> ArithResult<R>,
    support_signed: bool,
    support_unsigned: bool,
    commutative: bool,
}
impl<R: 'static> BinaryOp<R> {
    fn new(name: &'static str, range_op: fn(&IInterval, &IInterval) -> ArithResult<R>) -> Self {
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

    fn snapshot(&self)
    where
        R: WithInnerType,
    {
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
    fn snapshot_rhs_u32(&self)
    where
        R: WithInnerType,
    {
        let mut data = Vec::new();
        if self.support_signed {
            data.push((SNAPSHOT_RANGES_I8, SNAPSHOT_RANGES_U32));
            data.push((SNAPSHOT_RANGES_I32, SNAPSHOT_RANGES_U32));
        }
        if self.support_unsigned {
            data.push((SNAPSHOT_RANGES_U8, SNAPSHOT_RANGES_U32));
            data.push((SNAPSHOT_RANGES_U32, SNAPSHOT_RANGES_U32));
        }

        self.snapshot_with(&data);
    }
    fn snapshot_with(&self, inputs: &[(&[IInterval], &[IInterval])])
    where
        R: WithInnerType,
    {
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
fn test_strict_div_euclid() {
    let op = binary!(Arithmetic::strict_div_euclid);
    test_binary_all!(op, |lhs, rhs| lhs.checked_div_euclid(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_div_euclid() {
    let op = binary!(Arithmetic::wrapping_div_euclid);
    test_binary_all!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.wrapping_div_euclid(rhs))
    });
    op.snapshot();
}

#[test]
fn test_div_ceil() {
    let op = binary!(Arithmetic::div_ceil).only_unsigned();
    test_binary_unsigned!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.div_ceil(rhs))
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
fn test_strict_rem_euclid() {
    let op = binary!(Arithmetic::strict_rem_euclid);
    test_binary_all!(op, |lhs, rhs| lhs.checked_rem_euclid(rhs));
    op.snapshot();
}
#[test]
fn test_wrapping_rem_euclid() {
    let op = binary!(Arithmetic::wrapping_rem_euclid);
    test_binary_all!(op, |lhs, rhs| if rhs == 0 {
        None
    } else {
        Some(lhs.wrapping_rem_euclid(rhs))
    });
    op.snapshot();
}

#[test]
fn test_midpoint() {
    let op = binary!(Arithmetic::midpoint);
    test_binary_all!(op, |lhs, rhs| Some(lhs.midpoint(rhs)));
    op.snapshot();
}

#[test]
fn test_isqrt() {
    let op = unary!(Arithmetic::isqrt);
    #[allow(clippy::absurd_extreme_comparisons, unused_comparisons)]
    {
        test_unary_all!(op, |x| if x < 0 { None } else { Some(x.isqrt()) });
    }
    op.snapshot();
}

#[test]
fn test_ilog() {
    let op = binary!(Arithmetic::ilog);
    test_binary_all!(op, |lhs, rhs| lhs.checked_ilog(rhs));
    op.snapshot();
}
#[test]
fn test_ilog2() {
    let op = unary!(Arithmetic::ilog2);
    test_unary_all!(op, |x| x.checked_ilog2());
    op.snapshot();
}
#[test]
fn test_ilog10() {
    let op = unary!(Arithmetic::ilog10);
    test_unary_all!(op, |x| x.checked_ilog10());
    op.snapshot();
}

#[test]
fn test_saturating_pow() {
    let op = binary!(Arithmetic::saturating_pow);
    test_binary_all!(
        op,
        |lhs, rhs| Some(lhs.saturating_pow(rhs)),
        rhs = VALUES_U32
    );
    op.snapshot_rhs_u32();
}
#[test]
fn test_strict_pow() {
    let op = binary!(Arithmetic::strict_pow);
    test_binary_all!(op, |lhs, rhs| lhs.checked_pow(rhs), rhs = VALUES_U32);
    op.snapshot_rhs_u32();
}
#[test]
fn test_wrapping_pow() {
    let op = binary!(Arithmetic::wrapping_pow);
    test_binary_all!(op, |lhs, rhs| Some(lhs.wrapping_pow(rhs)), rhs = VALUES_U32);
    op.snapshot_rhs_u32();
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
fn test_unsigned_abs() {
    let op = unary!(Arithmetic::unsigned_abs).only_signed();
    test_unary_signed!(op, |x| Some(x.unsigned_abs()));
    op.snapshot();
}

#[test]
fn test_abs_diff() {
    let op = binary!(Arithmetic::abs_diff).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs.abs_diff(rhs)));
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
fn test_bitand() {
    let op = binary!(Arithmetic::bitand).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs & rhs));
    op.snapshot();
}
#[test]
fn test_bitor() {
    let op = binary!(Arithmetic::bitor).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs | rhs));
    op.snapshot();
}
#[test]
fn test_bitxor() {
    let op = binary!(Arithmetic::bitxor).commutative();
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
fn test_strict_shl() {
    let op = binary!(Arithmetic::strict_shl);
    test_binary_all!(op, |l, r| l.checked_shl(r), rhs = VALUES_U32);
    test_binary_all!(
        op,
        |l, r| l.checked_shl(u32::try_from(r).ok()?),
        rhs = VALUES_I32
    );
    test_binary_all!(
        op,
        |l, r| l.checked_shl(u32::try_from(r).ok()?),
        rhs = VALUES_U128
    );
    test_binary_all!(
        op,
        |l, r| l.checked_shl(u32::try_from(r).ok()?),
        rhs = VALUES_I128
    );
    op.snapshot_rhs_u32();
}
#[test]
fn test_wrapping_shl() {
    let op = binary!(Arithmetic::wrapping_shl);
    test_binary_all!(op, |l, r| Some(l.wrapping_shl(r)), rhs = VALUES_U32);
    test_binary_all!(op, |l, r| Some(l.wrapping_shl(r as u32)), rhs = VALUES_I32);
    test_binary_all!(op, |l, r| Some(l.wrapping_shl(r as u32)), rhs = VALUES_U128);
    test_binary_all!(op, |l, r| Some(l.wrapping_shl(r as u32)), rhs = VALUES_I128);
    op.snapshot_rhs_u32();
}
#[test]
fn test_unbounded_shl() {
    let op = binary!(Arithmetic::unbounded_shl);
    test_binary_all!(
        op,
        |lhs, rhs| Some(lhs.unbounded_shl(rhs)),
        rhs = VALUES_U32
    );
    op.snapshot_rhs_u32();
}

#[test]
fn test_strict_shr() {
    let op = binary!(Arithmetic::strict_shr);
    test_binary_all!(op, |l, r| l.checked_shr(r), rhs = VALUES_U32);
    test_binary_all!(
        op,
        |l, r| l.checked_shr(u32::try_from(r).ok()?),
        rhs = VALUES_I32
    );
    test_binary_all!(
        op,
        |l, r| l.checked_shr(u32::try_from(r).ok()?),
        rhs = VALUES_U128
    );
    test_binary_all!(
        op,
        |l, r| l.checked_shr(u32::try_from(r).ok()?),
        rhs = VALUES_I128
    );
    op.snapshot_rhs_u32();
}
#[test]
fn test_wrapping_shr() {
    let op = binary!(Arithmetic::wrapping_shr);
    test_binary_all!(op, |l, r| Some(l.wrapping_shr(r)), rhs = VALUES_U32);
    test_binary_all!(op, |l, r| Some(l.wrapping_shr(r as u32)), rhs = VALUES_I32);
    test_binary_all!(op, |l, r| Some(l.wrapping_shr(r as u32)), rhs = VALUES_U128);
    test_binary_all!(op, |l, r| Some(l.wrapping_shr(r as u32)), rhs = VALUES_I128);
    op.snapshot_rhs_u32();
}
#[test]
fn test_unbounded_shr() {
    let op = binary!(Arithmetic::unbounded_shr);
    test_binary_all!(
        op,
        |lhs, rhs| Some(lhs.unbounded_shr(rhs)),
        rhs = VALUES_U32
    );
    op.snapshot_rhs_u32();
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

#[test]
fn test_strict_next_power_of_two() {
    let op = unary!(Arithmetic::strict_next_power_of_two).only_unsigned();
    test_unary_unsigned!(op, |x| x.checked_next_power_of_two());
    op.snapshot();
}
#[test]
fn test_wrapping_next_power_of_two() {
    let op = unary!(Arithmetic::wrapping_next_power_of_two).only_unsigned();
    test_unary_unsigned!(op, |x| Some(x.checked_next_power_of_two().unwrap_or(0)));
    op.snapshot();
}

#[test]
fn test_strict_next_multiple_of() {
    let op = binary!(Arithmetic::strict_next_multiple_of).only_unsigned();
    test_binary_unsigned!(op, |lhs, rhs| lhs.checked_next_multiple_of(rhs));
    op.snapshot();
}

#[test]
fn test_signum() {
    let op = unary!(Arithmetic::signum).only_signed();
    test_unary_signed!(op, |x| Some(x.signum()));
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

macro_rules! test_cast_as {
    ($name:ident, $t:ty) => {
        #[test]
        fn $name() {
            let mut op = unary!(|x| Arithmetic::cast_as(x, <$t>::int_type()));
            op.name = concat!("cast_as ", stringify!($t));
            test_unary_all!(op, |x| Some(x as $t));
            op.snapshot();
        }
    };
}
test_cast_as!(test_cast_as_i8, i8);
test_cast_as!(test_cast_as_i16, i16);
test_cast_as!(test_cast_as_i32, i32);
test_cast_as!(test_cast_as_i64, i64);
test_cast_as!(test_cast_as_i128, i128);
test_cast_as!(test_cast_as_u8, u8);
test_cast_as!(test_cast_as_u16, u16);
test_cast_as!(test_cast_as_u32, u32);
test_cast_as!(test_cast_as_u64, u64);
test_cast_as!(test_cast_as_u128, u128);

macro_rules! test_case_into {
    ($name:ident, $t:ty) => {
        #[test]
        fn $name() {
            let mut op = unary!(|x| Arithmetic::cast_into(x, <$t>::int_type()));
            op.name = concat!("cast_into ", stringify!($t));
            test_unary_all!(op, |x| <$t>::try_from(x).ok());
            op.snapshot();
        }
    };
}
test_case_into!(test_cast_into_i8, i8);
test_case_into!(test_cast_into_i16, i16);
test_case_into!(test_cast_into_i32, i32);
test_case_into!(test_cast_into_i64, i64);
test_case_into!(test_cast_into_i128, i128);
test_case_into!(test_cast_into_u8, u8);
test_case_into!(test_cast_into_u16, u16);
test_case_into!(test_cast_into_u32, u32);
test_case_into!(test_cast_into_u64, u64);
test_case_into!(test_cast_into_u128, u128);

#[test]
fn test_eq() {
    let op = binary!(Arithmetic::eq).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs == rhs));
    op.snapshot();
}
#[test]
fn test_ne() {
    let op = binary!(Arithmetic::ne).commutative();
    test_binary_all!(op, |lhs, rhs| Some(lhs != rhs));
    op.snapshot();
}
#[test]
fn test_lt() {
    let op = binary!(Arithmetic::lt);
    test_binary_all!(op, |lhs, rhs| Some(lhs < rhs));
    op.snapshot();
}
#[test]
fn test_le() {
    let op = binary!(Arithmetic::le);
    test_binary_all!(op, |lhs, rhs| Some(lhs <= rhs));
    op.snapshot();
}
#[test]
fn test_gt() {
    let op = binary!(Arithmetic::gt);
    test_binary_all!(op, |lhs, rhs| Some(lhs > rhs));
    op.snapshot();
}
#[test]
fn test_ge() {
    let op = binary!(Arithmetic::ge);
    test_binary_all!(op, |lhs, rhs| Some(lhs >= rhs));
    op.snapshot();
}
