package collider.java;

/// Where a moving body ends, the velocity it keeps and whether it
/// ends on the ground.
public final class Move {

    public final V3 pos;
    public final V3 vel;
    public final boolean onGround;

    public Move(V3 pos, V3 vel, boolean onGround) {
        this.pos = pos;
        this.vel = vel;
        this.onGround = onGround;
    }
}
