
package calculation.physics

import domain.vector.AlgebraicVector

abstract class PotentialCalculator[VectorType <: AlgebraicVector[VectorType]] {
  def computeForceAndPotential(positionX: VectorType, positionY: VectorType): (VectorType, Double)
}
