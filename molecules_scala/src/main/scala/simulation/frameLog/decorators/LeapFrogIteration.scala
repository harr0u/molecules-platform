package simulation.frameLog.decorators

import calculation.physics.potentials.PairwisePotentialCalculator
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.actions._
import simulation.frameLog.FrameLog

// By the way, it looks strange: Leap From Integration algorithm + iterator pattern, can I cut off algo from progrmng?
trait LeapFrogIteration[V <: AlgebraicVector[V], F[_]] extends FrameLog[V, F] {

  def `∆t`: Double = 0.0001
  private lazy val `∆t∆t`: Double = `∆t` * `∆t`

  def potentialCalculator: PairwisePotentialCalculator[V]


  abstract override def initActions: Seq[ParticlesChangeAction[V]] = {
    updateForcesAndPotentials ++: super.initActions
  }

  abstract override def nextActions: Seq[ParticlesChangeAction[V]] = {
    Seq.concat(
      Seq(UpdatePositions((p) => p.position + p.velocity * `∆t` + p.acceleration * (`∆t∆t` / 2))),
      this.halfUpdateVelocities,
      this.updateForcesAndPotentials,
      this.halfUpdateVelocities,
      super.initActions
    )
  }

  private val halfUpdateVelocities: Seq[ParticlesChangeAction[V]] = {
    Seq(UpdateVelocities[V](p => p.velocity + p.acceleration * (`∆t` / 2)))
  }

  private val updateForcesAndPotentials: Seq[ParticlesChangeAction[V]] = Seq(
    ZeroForces(),
    ZeroPotentials(),
    UpdateForceAndPotential(recomputeForceAndPotential),
  )

  private def recomputeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double) = {
    potentialCalculator.computeForceAndPotential(particle1, particle2)
  }
}
