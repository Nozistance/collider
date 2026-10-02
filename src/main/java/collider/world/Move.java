package collider.world;

import collider.V3;

/// The end of one move of a body through the blocks. `dy` is how
/// far it moved up, as Entity.move hands it to checkFallDamage.
public record Move(V3 pos, V3 vel, boolean onGround, double dy) {}
