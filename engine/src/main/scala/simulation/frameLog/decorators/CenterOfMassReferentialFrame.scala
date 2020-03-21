package simulation.frameLog.decorators

import calculation.physics.CenterOfMassCalculator
import cats.{Monad, Traverse}
import cats.implicits._
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdateVelocities}
import simulation.frameLog.FrameLog

trait CenterOfMassReferentialFrame[
  V <: AlgebraicVector[V],
  Context[_],
  T[_],
  FL <: FrameLog[V, Context, T, FL]
] extends FrameLog[V, Context, T, FL] {
  override abstract def initActions: Context[Seq[ParticlesChangeAction[V]]] = {
    val cmAction = CenterOfMassCalculator
      .findCenterMassVelocity[V, Context, T](particles)
      .map(cmVelocity => UpdateVelocities[V](p => p.velocity - cmVelocity))
      .toSeq

    Context.map(super.initActions)(cmAction ++: _)
  }
}
