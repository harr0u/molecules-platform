package calculation.geometry.figures

sealed trait GeometricFigure
sealed trait CubicFigure extends GeometricFigure

case class Box(width: Double, length: Double, height: Double) extends CubicFigure
case class Cube(length: Double) extends CubicFigure

case class Sphere(radius: Double) extends GeometricFigure
