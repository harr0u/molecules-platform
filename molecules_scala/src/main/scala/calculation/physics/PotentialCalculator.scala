
package calculation.physics

import domain.geometry.vector.AlgebraicVector

abstract class PotentialCalculator[V <: AlgebraicVector[V]] {
  def computeForceAndPotentialWithDistance(distance: V): (V, Double)
}
