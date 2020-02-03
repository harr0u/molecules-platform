package calculation.limit_conditions

import calculation.geometry.figures.{Box, Cube, CubicFigure, GeometricFigure}
import domain.vector.Vector3D
import sun.security.util.Length

class BoxLimitConditions(override val boundaries: CubicFigure) extends LimitConditions[Vector3D, CubicFigure] {

  override def positionLimitCondition(position: Vector3D): Vector3D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      val coordinate_ = coordinate % length // -2 % 5 == -2
      if (coordinate_ < 0) length - coordinate_ else coordinate_ // -2 -> 3
    }

    this.applyLimitConditions(position, coordinateLimit)
  }

  override def distanceLimitCondition(distance: Vector3D): Vector3D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      if (Math.abs(coordinate) > (length / 2)) length - Math.abs(coordinate) else coordinate
    }

    this.applyLimitConditions(distance, coordinateLimit)
  }

  private def applyLimitConditions(vector: Vector3D, coordinateTransformer: (Double, Double) => Double): Vector3D = {
    val applyLimit = (width: Double, length: Double, height: Double) => Vector3D(
      coordinateTransformer(vector.x, width),
      coordinateTransformer(vector.y, length),
      coordinateTransformer(vector.z, height)
    )

    boundaries match {
      case Box(width, length, height) => applyLimit(width, length, height);
      case Cube(width) => applyLimit(width, width, width)
    }
  }
}
