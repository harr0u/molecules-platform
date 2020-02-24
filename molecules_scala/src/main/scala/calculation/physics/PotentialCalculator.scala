
package calculation.physics

import domain.Particle
import domain.geometry.vector.AlgebraicVector

abstract class PotentialCalculator[V <: AlgebraicVector[V]] {
  def computeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double)
}
