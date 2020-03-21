package simulation.frameLog

import cats.{Monad, Traverse}
import cats.implicits._
import domain.geometry.vector._
import simulation.actions.ParticlesChangeAction
import simulation.{ParticlesReducer, ParticlesState}


abstract class FrameLog[
  V <: AlgebraicVector[V],
  Context[_] : Monad,
  T[_] : Traverse,
  FL <: FrameLog[V, Context, T, FL]]{
  def init: Context[FL] = updateParticlesWithActions(this.initActions)
  def next: Context[FL] = updateParticlesWithActions(this.nextActions)

  def particles: ParticlesState[V, Context, T]

  protected def initActions: Context[Seq[ParticlesChangeAction[V]]] = Context.pure(Seq())
  protected def nextActions: Context[Seq[ParticlesChangeAction[V]]] = Context.pure(Seq())

  protected def particlesReducer: ParticlesReducer[V, Context, T]

  protected def updateParticlesWithActions(actions: Context[Seq[ParticlesChangeAction[V]]]): Context[FL] = {
      particlesReducer.applyChangeActions(particles)(actions)
        .flatMap(this.updateWithParticlesState)
  }

  protected def updateWithParticlesState(particles: ParticlesState[V, Context, T]): Context[FL]

  lazy val Context: Monad[Context] = implicitly[Monad[Context]]
}