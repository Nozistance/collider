package collider.world;

/// The block boxes that one sweep of a moving box meets. The boxes
/// may share the buffer of the thread, valid until its next sweep.
///
/// @param boxes The boxes, six coordinates for each box.
/// @param count How many boxes count, from the start of `boxes`.
public record Sweep(double[] boxes, int count) {}
