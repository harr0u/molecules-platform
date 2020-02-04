package calculation.geometry.figures

sealed trait GeometricFigure
sealed trait CubicFigure extends GeometricFigure {
  def width: Double
  def length: Double
  def height: Double
}

case class Box(width: Double, length: Double, height: Double) extends CubicFigure
case class Cube(length: Double) extends CubicFigure {
  override def width: Double = length
  override def height: Double = length
}


case class Sphere(radius: Double) extends GeometricFigure
