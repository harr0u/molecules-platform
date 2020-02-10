package calculation.limit_conditions

import domain.geometry.figures.{Square, Rectangle, RectangleFigure}
import domain.geometry.vector.Vector2D

class RectangleLimitConditions(override val boundaries: RectangleFigure) extends LimitConditions[Vector2D, RectangleFigure] {

  override def positionLimitCondition(position: Vector2D): Vector2D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      val coordinate_ = coordinate % length // -2 % 5 == -2
      if (coordinate_ < 0) length + coordinate_ else coordinate_ // -2 -> 3
    }

    this.applyLimitConditions(position, coordinateLimit)
  }

  override def distanceLimitCondition(distance: Vector2D): Vector2D = {
    val coordinateLimit = (coordinate: Double, length: Double) => {
      if (Math.abs(coordinate) > (length / 2)) {
        -1 * coordinate.sign * (length - Math.abs(coordinate))
      } else {
        coordinate
      }
    }

    this.applyLimitConditions(distance, coordinateLimit)
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
