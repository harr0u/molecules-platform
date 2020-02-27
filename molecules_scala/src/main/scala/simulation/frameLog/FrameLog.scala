package simulation.frameLog

import cats.{Functor, Monad}
import cats.implicits._
import domain.geometry.vector._
import simulation.actions.ParticlesChangeAction
import simulation.{ParticlesReducer, ParticlesState}


abstract class FrameLog[V <: AlgebraicVector[V], M[_] : Monad] {

  final def init: M[FrameLog[V, M]] = updateParticlesWithActions(this.initActions)
  final def next: M[FrameLog[V, M]] = updateParticlesWithActions(this.nextActions)

  def initActions: Seq[ParticlesChangeAction[V]] = Seq.empty[ParticlesChangeAction[V]]
  def nextActions: Seq[ParticlesChangeAction[V]] = Seq.empty[ParticlesChangeAction[V]]

  protected def updateWithParticles(particles: ParticlesState[V, M]): FrameLog[V, M]

  def particles: ParticlesState[V, M]
  def particlesReducer: ParticlesReducer[V, M]

  protected def updateParticlesWithActions(actions: Seq[ParticlesChangeAction[V]]): M[FrameLog[V, M]] = {
    particlesReducer.applyChangeActions(particles)(actions)
      .map(newParticles => this.updateWithParticles(particles = newParticles))
  }
}
