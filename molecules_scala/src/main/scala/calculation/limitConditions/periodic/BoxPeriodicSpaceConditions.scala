package calculation.limitConditions.periodic

import calculation.limitConditions.SpaceConditions
import domain.geometry.figures.{Box, Cube, CubicFigure}
import domain.geometry.vector.Vector3D

case class BoxPeriodicSpaceConditions(override val boundaries: CubicFigure) extends SpaceConditions[Vector3D, CubicFigure] {

  override def positionLimitCondition(position: Vector3D): Vector3D = {
    val coordinateLimit = (_coordinate: Double, length: Double) => {
      val coordinate = _coordinate % length // -2 % 5 == -2
      if (coordinate < 0) length + coordinate else coordinate // -2 -> 3
    }

    this.applyLimitConditions(position, coordinateLimit)
  }

  override def getDistanceBetween(point: Vector3D, other: Vector3D): Vector3D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      if (Math.abs(coordinate) > (length / 2)) {
        -1 * coordinate.sign * (length - Math.abs(coordinate))
      } else {
        coordinate
      }
    }

    this.applyLimitConditions(other - point, coordinateLimit)
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
