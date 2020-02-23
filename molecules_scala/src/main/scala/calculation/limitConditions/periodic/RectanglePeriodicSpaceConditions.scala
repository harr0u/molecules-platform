package calculation.limitConditions.periodic

import calculation.limitConditions.SpaceConditions
import domain.geometry.figures.{Rectangle, RectangleFigure, Square}
import domain.geometry.vector.Vector2D

case class RectanglePeriodicSpaceConditions(override val boundaries: RectangleFigure) extends SpaceConditions[Vector2D, RectangleFigure] {

  override def positionLimitCondition(position: Vector2D): Vector2D = {
    val coordinateLimit = (_coordinate: Double, length: Double) => {
      val coordinate = _coordinate % length // -2 % 5 == -2
      if (coordinate < 0) length + coordinate else coordinate // -2 -> 3
    }

    this.applyLimitConditions(position, coordinateLimit)
  }

  override def getDistanceBetween(point: Vector2D, other: Vector2D): Vector2D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      if (Math.abs(coordinate) > (length / 2)) {
        -1 * coordinate.sign * (length - Math.abs(coordinate))
      } else {
        coordinate
      }
    }

    this.applyLimitConditions(other - point, coordinateLimit)
  }

  private def applyLimitConditions(vector: Vector2D, coordinateTransformer: (Double, Double) => Double): Vector2D = {
    val applyLimit = (width: Double, length: Double) => Vector2D(
      coordinateTransformer(vector.x, width),
      coordinateTransformer(vector.y, length),
    )

    boundaries match {
      case Rectangle(width, length) => applyLimit(width, length);
      case Square(width) => applyLimit(width, width)
    }
  }
}
