package domain.geometry.figures

sealed trait GeometricFigure

sealed trait CubicFigure extends GeometricFigure {
  def width: Double
  def length: Double
  def height: Double
}

sealed trait RectangleFigure extends GeometricFigure {
  def width: Double
  def length: Double
}

case class Rectangle(width: Double, length: Double) extends RectangleFigure;
case class Square(length: Double) extends RectangleFigure {
  override def width: Double = length
}

case class Box(width: Double, length: Double, height: Double) extends CubicFigure
case class Cube(length: Double) extends CubicFigure {
  override def width: Double = length
  override def height: Double = length
}

case class Sphere(radius: Double) extends GeometricFigure
