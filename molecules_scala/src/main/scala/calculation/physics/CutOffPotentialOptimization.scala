package calculation.physics

import domain.Particle
import domain.geometry.vector.AlgebraicVector
import state.ParticlesSeqState

abstract trait CutOffPotentialOptimization[V <: AlgebraicVector[V]] extends PotentialCalculator[V] {
  def cutOffDistance: Double

  abstract override def computeForceAndPotentialWithDistance(distance: V): (V, Double) = {
    val length = distance.length;

    if (length > cutOffDistance) {
      (distance.zero, 0.0)
    } else {
      super.computeForceAndPotentialWithDistance(distance)
    }
  }

}
