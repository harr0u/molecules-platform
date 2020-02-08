package state

import cats.{Functor, Monad}
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import scala.concurrent.ExecutionContext.Implicits.global
import state.{ParticleReducer, ParticlesChangeAction, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.Future

class ParticlesPhysicsReducer[V <: AlgebraicVector[V]] extends ParticleReducer[V, Future] {

  override def applyChangeAction(state: ParticlesState[V, Future], action: ParticlesChangeAction[V]): Future[ParticlesState[V, Future]] = {
    action match {
      case act: ZeroForces[V] => this.applyZeroForcesActionForSeq(state, act)
      case act: ZeroPotentials[V] => this.applyZeroPotentialsActionForSeq(state, act)
      case act: UpdatePositions[V] => this.applyUpdatePositionsActionForSeq(state, act)
      case act: UpdateVelocities[V] => this.applyUpdateVelocitiesActionForSeq(state, act)
      case act: UpdateForceAndPotential[V] => this.applyUpdateForceAndPotentialActionForSeq(state, act)
    }
  }

  override def applyChangeActions(state: ParticlesState[V, Future], actions: Seq[ParticlesChangeAction[V]]): Future[ParticlesState[V, Future]] = {
    // In case of empty list we should get F[State] type, maybe make method to zip State to F[State],
    // Like unit for container of container
    actions.foldLeft(state.unit())((stateAcc, act) => stateAcc.flatMap((newState) => applyChangeAction(newState, act)))
  }

  protected def applyZeroForcesActionForSeq(particles: ParticlesState[V, Future], action: ZeroForces[V]): Future[ParticlesState[V, Future]] = {
    particles.map((p) => p.copy(force = p.force.zero))
  }

  protected def applyZeroPotentialsActionForSeq(particles: ParticlesState[V, Future], action: ZeroPotentials[V]): Future[ParticlesState[V, Future]] = {
    particles.map((p) => p.copy(potential = 0.0))
  }

  protected def applyUpdatePositionsActionForSeq(particles: ParticlesState[V, Future], action: UpdatePositions[V]): Future[ParticlesState[V, Future]] = {
    particles.map((p) => p.copy(position = action.fn(p)))
  }

  protected def applyUpdateVelocitiesActionForSeq(particles: ParticlesState[V, Future], action: UpdateVelocities[V]): Future[ParticlesState[V, Future]] = {
    particles.map((p) => p.copy(velocity = action.fn(p)))
  }

  protected def applyUpdateForceAndPotentialActionForSeq(particles: ParticlesState[V, Future], action: UpdateForceAndPotential[V]): Future[ParticlesState[V, Future]] = {
    particles.particlesReduce((p1, p2) => action.fn(p1, p2)._1)
  }
}