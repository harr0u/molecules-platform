package state

import domain.Particle
import domain.geometry.vector.AlgebraicVector

sealed trait ParticlesChangeAction[V <: AlgebraicVector[V]]

case class UpdatePositions[V <: AlgebraicVector[V]](fn: (Particle[V]) => V) extends ParticlesChangeAction[V]

case class UpdateVelocities[V <: AlgebraicVector[V]](fn: (Particle[V]) => V) extends ParticlesChangeAction[V]

case class ZeroForces[V <: AlgebraicVector[V]]() extends ParticlesChangeAction[V]

case class ZeroPotentials[V <: AlgebraicVector[V]]() extends ParticlesChangeAction[V]

case class UpdateForceAndPotential[V <: AlgebraicVector[V]](
  fn: (Particle[V], Particle[V]) => (Particle[V], Particle[V])
) extends ParticlesChangeAction[V]