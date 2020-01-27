package domain.vector

import domain.Particle
import domain.vector._
import scala.math.sqrt;


class Vector2D(val x: Double = 0.0, val y: Double = 0.0) extends AlgebraicVector[Vector2D] {
  def length: Double = sqrt(x*x + y*y)

  def scale(factor: Double): Vector2D = Vector2D(x * factor, y * factor)
  def *(factor: Double): Vector2D = scale(factor)
  def *:(factor: Double): Vector2D = scale(factor)

  def addVector(other: Vector2D): Vector2D = Vector2D(this.x + other.x, this.y + other.y)
  def +(other: Vector2D): Vector2D = addVector(other)

  def subVector(other: Vector2D): Vector2D = addVector(other * -1)
  def -(other: Vector2D): Vector2D = subVector(other)

  def zero: Vector2D = Vector2D()

  def mapCoordinates(mapFn: Double => Double): Vector2D = {
    Vector2D(mapFn(x), mapFn(y))
  }

  override def toString(): String = {
    s"(${this.x}, ${this.y})"
  }
}

object Vector2D {
    val empty: Vector2D = new Vector2D()
    def apply(x: Double = 0, y: Double = 0): Vector2D = {
        new Vector2D(x, y)
    }
}
