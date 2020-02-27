package calculation.physics.potentials

import domain.Particle
import domain.geometry.vector.AlgebraicVector

trait PairwisePotentialCalculator[V <: AlgebraicVector[V]] {
  def computeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double)
}
