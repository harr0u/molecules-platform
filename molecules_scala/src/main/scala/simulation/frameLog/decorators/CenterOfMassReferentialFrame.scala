package simulation.frameLog.decorators

import calculation.physics.CenterOfMassCalculator
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdateVelocities}
import simulation.frameLog.FrameLog

 trait CenterOfMassReferentialFrame[V <: AlgebraicVector[V], F[_]] extends FrameLog[V, F] {
  override abstract def initActions: Seq[ParticlesChangeAction[V]] = {
    CenterOfMassCalculator.findCenterMassVelocity[V, F](particles)
      .map(cmVelocity => UpdateVelocities[V](p => p.velocity - cmVelocity))
      .map(act => act +: super.initActions)
      .getOrElse(super.initActions)
  }
}
