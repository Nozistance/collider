package collider.java;

/// The block boxes that one sweep of a moving box meets.
///
/// @param a The boxes, six coordinates for each box.
/// @param n The number of boxes.
public record Sweep(double[] a, int n) {}
