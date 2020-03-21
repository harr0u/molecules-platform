package calculation.space

import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector

abstract class SpaceConditions[VectorType <: AlgebraicVector[VectorType], FigureType <: GeometricFigure] {
  def positionLimitCondition(position: VectorType): VectorType
  def getDistanceBetween(point: VectorType, other: VectorType): VectorType

  val boundaries: FigureType
}
