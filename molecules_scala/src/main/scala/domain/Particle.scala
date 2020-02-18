package domain

import domain.geometry.vector.{AlgebraicVector, Vector2D}

case class Particle[V <: AlgebraicVector[V]](id: Int, position: V, velocity: V, force: V, potential: Double, mass: Double) {
  private val invertedMass: Double = 1.0 / mass
  def acceleration: V = this.force * invertedMass
}
