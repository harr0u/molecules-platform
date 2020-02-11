package calculation.physics

import domain.Particle
import domain.geometry.vector.AlgebraicVector
import state.ParticlesState

object CenterOfMassCalculator {
  def findCenterMassVelocity[V <: AlgebraicVector[V], F[_]](state: ParticlesState[V, F]): Option[V] = {
    state.counit.foldLeft[Option[(V, Double)]](None)((acc : Option[(V, Double)], p: Particle[V]) => {
      val (momentumAcc, massAcc) = acc.getOrElse((p.velocity.zero, 0.0))

      Some((momentumAcc + (p.mass *: p.velocity), massAcc + p.mass))
    }).flatMap((acc) => {
      val (momentum: V, mass: Double) = acc
      if (mass > 0) Some((momentum * (1 / mass))) else None
    })
  }

  def findCenterMassPosition[V <: AlgebraicVector[V], F[_]](state: ParticlesState[V, F]): Option[V] = {
    state.counit.foldLeft[Option[(V, Double)]](None)((acc : Option[(V, Double)], p: Particle[V]) => {
      val (massPositionAcc, massAcc) = acc.getOrElse((p.velocity.zero, 0.0))

      Some((massPositionAcc + (p.mass *: p.position), massAcc + p.mass))
    }).flatMap((acc) => {
      val (massPositionAcc: V, mass: Double) = acc
      if (mass > 0) Some((massPositionAcc * (1 / mass))) else None
    })
  }
}
