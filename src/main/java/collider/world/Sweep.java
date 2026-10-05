package collider.world;

/// The block boxes that one sweep of a moving box meets. They stay
/// valid only until the next sweep on the same thread.
/// In Java because Phys, a Java class, returns it.
///
/// @param boxes The boxes, six coordinates for each box.
/// @param count How many boxes count, from the start of `boxes`.
public record Sweep(double[] boxes, int count) {}
