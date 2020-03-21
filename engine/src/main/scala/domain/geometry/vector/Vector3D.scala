package domain.geometry.vector

import domain.Particle
import domain.geometry.vector._
import scala.math.sqrt


case class Vector3D(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) extends AlgebraicVector[Vector3D] {
  def squaredLength: Double = x*x + y*y + z*z

  def scale(factor: Double): Vector3D = Vector3D(x * factor, y * factor, z * factor)
  def *(factor: Double): Vector3D = scale(factor)
  def *:(factor: Double): Vector3D = scale(factor)

  def addVector(other: Vector3D): Vector3D = Vector3D(this.x + other.x, this.y + other.y, this.z + other.z)
  def +(other: Vector3D): Vector3D = addVector(other)

  def subVector(other: Vector3D): Vector3D = addVector(other * -1.0)
  def -(other: Vector3D): Vector3D = subVector(other)

  def zero: Vector3D = Vector3D()

  def mapCoordinates(mapFn: Double => Double): Vector3D = {
    mapiCoordinates((_, coord) => mapFn(coord))
  }

  def mapiCoordinates(mapFn: (Int, Double) => Double): Vector3D = {
    Vector3D(mapFn(0, x), mapFn(1, y), mapFn(2, z))
  }
}

object Vector3D {
    val empty: Vector3D = Vector3D()
}