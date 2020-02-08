package calculation.physics

import domain.geometry.vector.AlgebraicVector

class LennardJonesCutOffPotential[V <: AlgebraicVector[V]](val cutOffRadius: Double = 5.0) extends LennardJonesPotential[V] {
  override def computeForceAndPotential(distance: V): (V, Double) = {
    val length = distance.length;

    if (length > cutOffRadius) (distance.zero, 0.0) else super.computeForceAndPotential(distance)
  }

}
