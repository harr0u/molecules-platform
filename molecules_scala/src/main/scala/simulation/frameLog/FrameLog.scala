package simulation.frameLog

import cats.Monad
import cats.implicits._
import domain.geometry.vector._
import simulation.actions.ParticlesChangeAction
import simulation.{ParticlesReducer, ParticlesState}


abstract class FrameLog[V <: AlgebraicVector[V], Context[_] : Monad, T[_]]{
  final def init: Context[FrameLog[V, Context, T]] = updateParticlesWithActions(this.initActions)
  final def next: Context[FrameLog[V, Context, T]] = updateParticlesWithActions(this.nextActions)

  def particles: ParticlesState[V, Context, T]

  protected def initActions: Context[Seq[ParticlesChangeAction[V]]] = Context.pure(Seq())
  protected def nextActions: Context[Seq[ParticlesChangeAction[V]]] = Context.pure(Seq())

  protected def particlesReducer: ParticlesReducer[V, Context, T]

  protected def updateParticlesWithActions(actions: Context[Seq[ParticlesChangeAction[V]]]): Context[FrameLog[V, Context, T]] = {
      particlesReducer.applyChangeActions(particles)(actions)
        .map((newParticles: ParticlesState[V, Context, T]) => this.updateWithParticles(particles = newParticles))
  }

  protected def updateWithParticles(particles: ParticlesState[V, Context, T]): FrameLog[V, Context, T]

  lazy val Context: Monad[Context] = implicitly[Monad[Context]]
}
