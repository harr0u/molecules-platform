package calculation.limit_conditions

import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector

abstract class LimitConditions[VectorType <: AlgebraicVector[VectorType], FigureType <: GeometricFigure] {
  def positionLimitCondition(position: VectorType): VectorType
  def distanceLimitCondition(distance: VectorType): VectorType

  val boundaries: FigureType
}
