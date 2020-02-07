package domain.geometry.vector

import domain.Particle
import domain.geometry.vector._
import scala.math.sqrt


case class Vector2D(x: Double = 0.0, y: Double = 0.0) extends AlgebraicVector[Vector2D] {
  def length: Double = sqrt(x * x + y * y)

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

//  override def toString: String = {
//    s"($x, $y)"
//  }
}

//object LL {
//
//  trait VectorOps[A] {
//    def plus(left: A, right: A): A
//  }
//
//  implicit class VectorSyntaxOps[A](a: A)(implicit ops: VectorOps[A]) {
//    def +(right: A) = ops.plus(a, right)
//  }
//
//}

object Vector2D {
  val empty: Vector2D = new Vector2D()

//  def main(args: Array[String]): Unit = {
//    val a = 2;
//    val b = a + 2;
//  }
}
