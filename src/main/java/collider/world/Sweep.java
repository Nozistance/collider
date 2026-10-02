package collider.world;

/// The block boxes that one sweep of a moving box meets.
///
/// @param a The boxes, six coordinates for each box.
public record Sweep(double[] a, int n) {}
