package state

import domain.Particle
import domain.geometry.vector.AlgebraicVector
import state.UpdateForceAndPotential.{ForceAndPotentialCalculator, ReduceParticles}

sealed trait ParticlesChangeAction[V <: AlgebraicVector[V]]

class ParticleActionMap[V <: AlgebraicVector[V]](val mapFn: Particle[V] => Particle[V])
  extends ParticlesChangeAction[V] {

  def andThen(thenMap: ParticleActionMap[V]): ParticleActionMap[V] = {
    new ParticleActionMap[V](p => thenMap.mapFn(this.mapFn(p)))
  }
}

case class UpdatePositions[V <: AlgebraicVector[V]](fn: (Particle[V]) => V)
  extends ParticleActionMap[V]((particle) => particle.copy(position = fn(particle)))

case class UpdateVelocities[V <: AlgebraicVector[V]](fn: (Particle[V]) => V)
  extends ParticleActionMap[V]((particle) => particle.copy(velocity = fn(particle)))

case class ZeroForces[V <: AlgebraicVector[V]]()
  extends ParticleActionMap[V]((particle) => particle.copy(force = particle.force.zero))

case class ZeroPotentials[V <: AlgebraicVector[V]]()
  extends ParticleActionMap[V]((particle) => particle.copy(potential = 0.0))

class ParticleActionReduce[V <: AlgebraicVector[V]](val reduceFn: ReduceParticles[V])
  extends ParticlesChangeAction[V]

case class UpdateForceAndPotential[V <: AlgebraicVector[V]](fn: ForceAndPotentialCalculator[V])
  extends ParticleActionReduce[V](UpdateForceAndPotential.fnToReduceFn(fn))

object UpdateForceAndPotential {
  type ForceAndPotentialCalculator[V <: AlgebraicVector[V]] = (Particle[V], Particle[V]) => (V, Double)
  type ReduceParticles[V <: AlgebraicVector[V]] = (Particle[V], Particle[V]) => Particle[V]

  def fnToReduceFn[V <: AlgebraicVector[V]]: (ForceAndPotentialCalculator[V]) => ReduceParticles[V] = (fn) => {
    (particle, other) => {
      val (force, potential) = fn(particle, other)

      particle.copy(force = particle.force + force, potential = particle.potential + potential)
    }
  }
}