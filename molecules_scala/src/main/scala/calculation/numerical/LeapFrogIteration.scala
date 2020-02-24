package calculation.numerical

import domain.geometry.figures.GeometricFigure
import calculation.limitConditions.SpaceConditions
import calculation.physics.{CenterOfMassCalculator, LennardJonesPotential, PotentialCalculator}
import domain.Particle
import domain.geometry.vector._
import state.{ParticleActionMap, ParticleReducer, ParticlesChangeAction, ParticlesState, ParticlesStateReducer, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats.implicits._


// By the way, it looks strange: Leap From Integration algorithm + iterator pattern, can I cut off algo from progrmng?
case class LeapFrogIteration[V <: AlgebraicVector[V], Fig <: GeometricFigure](`∆t`: Double = 0.0001)
                                                                             (implicit potentialCalculator: LennardJonesPotential[V]) extends TimeIntegrator[V] {
  def init: Seq[ParticlesChangeAction[V]] = updateForcesAndPotentials

  private val `∆t∆t`: Double = `∆t` * `∆t`
  def iterationStep: Seq[ParticlesChangeAction[V]] = {
    Seq.concat(
      Seq(UpdatePositions((p) => p.position + p.velocity * `∆t` + p.acceleration * (`∆t∆t` / 2))),
      this.halfUpdateVelocities,
      this.updateForcesAndPotentials,
      this.halfUpdateVelocities
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