package collider.world;

import collider.V3;

/// The end of one move of a body through the blocks.
///
/// @param pos Where the body ends.
/// @param vel The velocity it keeps.
/// @param onGround True when it landed on a block.
/// @param dy How far it moved up, below zero when it moved down.
public record Move(V3 pos, V3 vel, boolean onGround, double dy) {}
