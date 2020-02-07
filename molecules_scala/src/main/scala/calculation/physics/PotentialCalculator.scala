
package calculation.physics

import domain.geometry.vector.AlgebraicVector

abstract class PotentialCalculator[VectorType <: AlgebraicVector[VectorType]] {
  def computeForceAndPotential(distance: VectorType): (VectorType, Double)
}
