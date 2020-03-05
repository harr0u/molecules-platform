package simulation.frameLog

import cats.{Monad}
import cats.implicits._
import domain.geometry.vector._
import simulation.actions.ParticlesChangeAction
import simulation.{ParticlesReducer, ParticlesState}


abstract class FrameLog[V <: AlgebraicVector[V], M[_] : Monad, T[_]] {
  final def init: M[FrameLog[V, M, T]] = updateParticlesWithActions(this.initActions)
  final def next: M[FrameLog[V, M, T]] = updateParticlesWithActions(this.nextActions)

  def particles: ParticlesState[V, M, T]

  protected def initActions: Seq[ParticlesChangeAction[V]]
  protected def nextActions: Seq[ParticlesChangeAction[V]]

  protected def particlesReducer: ParticlesReducer[V, M, T]
  protected def updateWithParticles(particles: ParticlesState[V, M, T]): FrameLog[V, M, T]

  protected def updateParticlesWithActions(actions: Seq[ParticlesChangeAction[V]]): M[FrameLog[V, M, T]] = {
    particlesReducer.applyChangeActions(particles)(actions)
      .map(newParticles => this.updateWithParticles(particles = newParticles))
  }
}
