package collider.world;

/// The y coordinates of collision shapes, as `VoxelShape.getCoords`
/// gives them on the y axis, grid points between the edges included.
///
/// @param states The coordinates of each block state as an empty
///     context meets it, null for those of a full cube.
/// @param block The coordinates of a full cube.
/// @param scaffoldingBottom Those of the bottom of hanging
///     scaffolding.
/// @param snowFalling Those of powder snow under a falling body.
public record YCoords(Object[] states, double[] block,
        double[] scaffoldingBottom, double[] snowFalling) {}
