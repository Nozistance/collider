package collider;

/// A block cell packed into one long as the game packs it: x and z in
/// 26 bits each, high to low x, z, y, and y in the low 12 bits.
public final class Cell {

    private static final long XZ_MASK = (1L << 26) - 1;

    private static final long Y_MASK = (1L << 12) - 1;

    private static final int Z_SHIFT = 12;

    private static final int X_SHIFT = 38;

    public static long pack(long x, long y, long z) {
        return (x & XZ_MASK) << X_SHIFT | (z & XZ_MASK) << Z_SHIFT | (y & Y_MASK);
    }

    public static long x(long c) {
        return c >> X_SHIFT;
    }

    public static long y(long c) {
        return c << 52 >> 52;
    }

    public static long z(long c) {
        return c << 26 >> X_SHIFT;
    }

    /// Returns the cell `dx`, `dy`, `dz` away from `c`. Each coordinate
    /// wraps inside its own field.
    public static long offset(long c, long dx, long dy, long dz) {
        return pack(x(c) + dx, y(c) + dy, z(c) + dz);
    }
}
