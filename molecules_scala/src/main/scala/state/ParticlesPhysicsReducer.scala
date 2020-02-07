package state

import domain.Particle
import domain.geometry.vector.AlgebraicVector
import state.{ParticleReducer, ParticlesChangeAction, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

class ParticlesPhysicsReducer[V <: AlgebraicVector[V]] extends ParticleReducer[V] {

  override def applyChangeAction(state: ParticlesState[V], action: ParticlesChangeAction[V]): ParticlesState[V] = {
    action match {
      case act: ZeroForces[V] => this.applyZeroForcesActionForSeq(state, act)
      case act: ZeroPotentials[V] => this.applyZeroPotentialsActionForSeq(state, act)
      case act: UpdatePositions[V] => this.applyUpdatePositionsActionForSeq(state, act)
      case act: UpdateVelocities[V] => this.applyUpdateVelocitiesActionForSeq(state, act)
      case act: UpdateForceAndPotential[V] => this.applyUpdateForceAndPotentialActionForSeq(state, act)
    }
  }

  override def applyChangeActions(particlesContainer: ParticlesState[V], actions: Seq[ParticlesChangeAction[V]]): ParticlesState[V] = {
    actions.foldLeft(particlesContainer)(applyChangeAction)
  }

  protected def applyZeroForcesActionForSeq(particles: ParticlesState[V], action: ZeroForces[V]): ParticlesState[V] = {
    particles.map((p) => p.copy(force = p.force.zero))
  }

  protected def applyZeroPotentialsActionForSeq(particles: ParticlesState[V], action: ZeroPotentials[V]): ParticlesState[V] = {
    particles.map((p) => p.copy(potential = 0.0))
  }

  protected def applyUpdatePositionsActionForSeq(particles: ParticlesState[V], action: UpdatePositions[V]): ParticlesState[V] = {
    particles.map((p) => p.copy(position = action.fn(p)))
  }

  protected def applyUpdateVelocitiesActionForSeq(particles: ParticlesState[V], action: UpdateVelocities[V]): ParticlesState[V] = {
    particles.map((p) => p.copy(velocity = action.fn(p)))
  }

  protected def applyUpdateForceAndPotentialActionForSeq(particles: ParticlesState[V], action: UpdateForceAndPotential[V]): ParticlesState[V] = {
    particles.particlesReduce((p1, p2) => action.fn(p1, p2)._1)
    //    (for {
//      (p1, i) <- particles.particlesStream.zipWithIndex
//      (p2, j) <- particles.particlesStream.zipWithIndex
//      if i < j
//    } yield (p1, p2))
//      .foldLeft(Map[Int, Particle[V]]())((acc: Map[Int, Particle[V]], particlesPair) => {
//        val (particle1, particle2) = particlesPair
//        val (newParticle1, newParticle2) = action.fn(particle1, particle2)
//
//        val mergeParticle = (acc: Map[Int, Particle[V]], newParticle: Particle[V]) => {
//          val mergedParticle: Particle[V] = acc
//            .get(newParticle.id)
//            .map((p) => p.copy(
//              force = p.force + newParticle.force,
//              potential = p.potential + newParticle.potential
//            ))
//            .getOrElse(newParticle)
//
//          acc + (mergedParticle.id -> mergedParticle)
//        }
//
//        mergeParticle(mergeParticle(acc, newParticle1), newParticle2)
//      }).values
  }
}

object ParticlesPhysicsReducer {
  // In general particles container may use particles positions to sort them oslt
  // That's why reducer works with Container[VecType, Particle[_]]
  // As I understand, i generic
  type ParticlesSeqContainer[V, P[_]] = Seq[P[V]]
}