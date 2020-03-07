package simulation.actions

import domain.Particle
import domain.geometry.vector.AlgebraicVector

sealed trait ParticlesChangeAction[V <: AlgebraicVector[V]]

class ParticleActionMap[V <: AlgebraicVector[V]](val mapFn: Particle[V] => Particle[V])
  extends ParticlesChangeAction[V] {

  // Looks like semigroup, moreover Wrapping return of mapFn will make
  // ParticleActionMap ~>[F, P[V], P[V]]
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

class ParticleActionReduce[V <: AlgebraicVector[V]](val reduceFn: (Particle[V], Particle[V]) => Particle[V])
  extends ParticlesChangeAction[V]

case class UpdateForceAndPotential[V <: AlgebraicVector[V]](fn: (Particle[V], Particle[V]) => (V, Double))
  extends ParticleActionReduce[V](UpdateForceAndPotential.potentialFnToReduceFn(fn))

object UpdateForceAndPotential {
  def potentialFnToReduceFn[V <: AlgebraicVector[V]]: ((Particle[V], Particle[V]) => (V, Double)) => (Particle[V], Particle[V]) => Particle[V] = (fn) => {
    (particle, other) => {
      val (force, potential) = fn(particle, other)
      val a: UpdatePositions[V] = UpdatePositions[V](p => p.position)

      particle.copy(force = particle.force + force, potential = particle.potential + potential)
    }
  }
}