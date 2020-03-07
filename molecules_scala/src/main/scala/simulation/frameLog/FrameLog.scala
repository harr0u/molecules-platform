package simulation.frameLog

import cats.{Monad}
import cats.implicits._
import domain.geometry.vector._
import simulation.actions.ParticlesChangeAction
import simulation.{ParticlesReducer, ParticlesState}


abstract class FrameLog[V <: AlgebraicVector[V], Context[_] : Monad, T[_]] {
  final def init: Context[FrameLog[V, Context, T]] = updateParticlesWithActions(this.initActions)
  final def next: Context[FrameLog[V, Context, T]] = updateParticlesWithActions(this.nextActions)

  def particles: ParticlesState[V, Context, T]

  protected def initActions: Seq[ParticlesChangeAction[V]] = Seq()
  protected def nextActions: Seq[ParticlesChangeAction[V]] = Seq()

  protected def particlesReducer: ParticlesReducer[V, Context, T]
  protected def updateWithParticles(particles: ParticlesState[V, Context, T]): FrameLog[V, Context, T]

  protected def updateParticlesWithActions(actions: Seq[ParticlesChangeAction[V]]): Context[FrameLog[V, Context, T]] = {
    particlesReducer.applyChangeActions(particles)(actions)
      .map(newParticles => this.updateWithParticles(particles = newParticles))
  }
}
