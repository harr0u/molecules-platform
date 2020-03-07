package simulation.frameLog.decorators

import calculation.physics.CenterOfMassCalculator
import cats.Traverse
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdateVelocities}
import simulation.frameLog.FrameLog

 trait CenterOfMassReferentialFrame[V <: AlgebraicVector[V], Context[_], T[_]] extends FrameLog[V, Context, T] {
  override abstract def initActions: Seq[ParticlesChangeAction[V]] = {
    val cmAction = CenterOfMassCalculator
      .findCenterMassVelocity[V, Context, T](particles)
      .map(cmVelocity => UpdateVelocities[V](p => p.velocity - cmVelocity))
      .toSeq

    cmAction ++: super.initActions
  }
}
