package calculation.physics.potentials

import domain.geometry.vector.AlgebraicVector

abstract trait CutOffPotentialOptimization[V <: AlgebraicVector[V]] extends LennardJonesPotential[V] {
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
