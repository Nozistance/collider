package collider;

/// Random numbers hashed from longs. The same longs always give the
/// same number.
public final class RandomSupport {

    /// Returns `z` mixed so that near longs give far apart ones.
    public static long mixStafford13(long z) {
        z = (z ^ (z >>> 30)) * -4658895280553007687L;
        z = (z ^ (z >>> 27)) * -7723592293110705685L;
        return z ^ (z >>> 31);
    }

    /// Returns a number from 0 to 1 for `a`, `b` and `c`.
    public static double unit(long a, long b, long c) {
        long h = mixStafford13(mixStafford13(mixStafford13(a) + b) + c);
        return (double) (h & 0xFFFFFF) / 1.6777216E7;
    }

    /// Returns a number from 0 to 1 for `a`, `b`, `c` and `d`.
    public static double unit(long a, long b, long c, long d) {
        return unit(a, b, 31 * c + d);
    }
}
