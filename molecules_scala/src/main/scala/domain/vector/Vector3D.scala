package domain.vector

import domain.Particle
import domain.vector._
import scala.math.sqrt;


class Vector3D(val x: Double = 0.0, val y: Double = 0.0, val z: Double = 0.0) extends AlgebraicVector[Vector3D] {
  def length: Double = sqrt(x*x + y*y + z*z)

  def scale(factor: Double): Vector3D = Vector3D(x * factor, y * factor, z * factor)
  def *(factor: Double): Vector3D = scale(factor)
  def *:(factor: Double): Vector3D = scale(factor)

  def addVector(other: Vector3D): Vector3D = Vector3D(this.x + other.x, this.y + other.y, this.z + other.z)
  def +(other: Vector3D): Vector3D = addVector(other)

  def subVector(other: Vector3D): Vector3D = addVector(other * -1.0)
  def -(other: Vector3D): Vector3D = subVector(other)

  def zero: Vector3D = Vector3D()

  def mapCoordinates(mapFn: Double => Double): Vector3D = {
    Vector3D(mapFn(x), mapFn(y), mapFn(z))
  }

  override def toString(): String = {
    s"(${this.x}, ${this.y}, ${this.z})"
  }
}

object Vector3D {
    val empty: Vector3D = Vector3D()
    def apply(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vector3D = {
        new Vector3D(x, y, z)
    }
}