package simulation.frameLog.decorators

import calculation.physics.CenterOfMassCalculator
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdateVelocities}
import simulation.frameLog.FrameLog

 trait CenterOfMassReferentialFrame[V <: AlgebraicVector[V], F[_]] extends FrameLog[V, F] {
  override abstract def initActions: Seq[ParticlesChangeAction[V]] = {
    val cmAction = CenterOfMassCalculator
      .findCenterMassVelocity[V, F](particles)
      .map(cmVelocity => UpdateVelocities[V](p => p.velocity - cmVelocity))
      .toSeq

    cmAction ++: super.initActions
  }
}
