package domain

import domain.vector.AlgebraicVector

case class Particle[V <: AlgebraicVector[V]](
  id: Int,
  position: V,
  velocity: V,
  force: V,
  potential: Double,
  mass: Double
) {
  private val invertedMass: Double = 1.0 / mass
  def acceleration: V = this.force * invertedMass
}

object Particle {
  var nextParticleId: Int = 0
}