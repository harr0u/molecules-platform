package domain

import domain.vector.AlgebraicVector

class Particle[VectorType <: AlgebraicVector[VectorType]](var position: VectorType,
                                                          var velocity: VectorType,
                                                          val mass: Double = 1.0) {
  val id: Int = Particle.nextParticleId;
  Particle.nextParticleId += 1

  var potential: Double = 0.0

  var force: VectorType = position.zero

  private val invertedMass: Double = 1.0 / mass
  def acceleration: VectorType = {force * invertedMass}

  def clearForceAndPotential(): Unit = {
    force = force.zero
    potential = 0.0
  }

  override def toString: String = {
    s"Particle(${position.toString}, ${velocity.toString()})"
  }
}

object Particle {
  private var nextParticleId: Int = 0
}