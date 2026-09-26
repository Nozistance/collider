package collider.java;

/// The block boxes one sweep of a moving box gathers, `n` of them in
/// `a`, each stored as 6 consecutive doubles.
public final class Sweep {

    public final double[] a;
    public final int n;

    public Sweep(double[] a, int n) {
        this.a = a;
        this.n = n;
    }
}
