package domain.geometry.vector


abstract class AlgebraicVector[VectorType <: AlgebraicVector[VectorType]] {
  def length: Double = Math.sqrt(squaredLength)
  def squaredLength: Double

  def scale(factor: Double): VectorType
  def *(factor: Double): VectorType
  def *:(factor: Double): VectorType

  def addVector(other: VectorType): VectorType
  def +(other: VectorType): VectorType

  def subVector(other: VectorType): VectorType
  def -(other: VectorType): VectorType

  def mapCoordinates(mapFn: (Double) => Double): VectorType
  def mapiCoordinates(mapFn: (Int, Double) => Double): VectorType

  def zero: VectorType
}
