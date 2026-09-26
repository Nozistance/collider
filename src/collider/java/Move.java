package collider.java;

/// The end of one move of a body through the blocks.
///
/// @param pos The position where the body ends.
/// @param vel The velocity that the body keeps.
/// @param onGround True when the body ends on the ground.
public record Move(V3 pos, V3 vel, boolean onGround) {}
