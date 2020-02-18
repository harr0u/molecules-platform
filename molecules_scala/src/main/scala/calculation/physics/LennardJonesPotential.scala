package calculation.physics

import domain.Particle
import domain.geometry.vector._

class LennardJonesPotential[VectorType <: AlgebraicVector[VectorType]] extends PotentialCalculator[VectorType] {
  def computeForceAndPotential(distance: VectorType): (VectorType, Double) = {
    val distanceSquaredInverted: Double = 1.0 / distance.squaredLength
    val r6: Double = Math.pow(distanceSquaredInverted, 3)
    val r12: Double = r6 * r6

    // Read More: https://en.wikipedia.org/wiki/Lennard-Jones_potential#Dimensionless_(reduced)_units
    val force: VectorType = distance * (-48 * (r12 * distanceSquaredInverted - 0.5 * r6 * distanceSquaredInverted))
    val potential: Double = 2 * (r12 - r6)

    (force, potential)
  }
}