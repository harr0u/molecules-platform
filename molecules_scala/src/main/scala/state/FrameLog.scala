package calculation.numerical

import domain.geometry.figures.GeometricFigure
import calculation.limitConditions.SpaceConditions
import calculation.physics.{CenterOfMassCalculator, LennardJonesPotential, PotentialCalculator}
import cats.Monad
import domain.Particle
import domain.geometry.vector._
import state.{ParticleActionMap, ParticleReducer, ParticlesChangeAction, ParticlesState, ParticlesStateReducer, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats.implicits._


// By the way, it looks strange: Leap From Integration algorithm + iterator pattern, can I cut off algo from progrmng?
case class FrameLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, M[_]](
                                                                               particles: ParticlesState[V, M],
                                                                               particlesReducer: ParticleReducer[V, M],
                                                                               limitConditions: SpaceConditions[V, Fig],
                                                                               integrationAlgorithm: TimeIntegrator[V],
                                                                               `∆t`: Double = 0.0001
                                                                             )(implicit M : Monad[M], potentialCalculator: PotentialCalculator[V]) {

  def init: M[FrameLog[V, Fig, M]] = {
    val centerOfMassAction: Seq[ParticlesChangeAction[V]] = Seq(new ParticleActionMap[V](
      (for {
        cmVelocity <- CenterOfMassCalculator.findCenterMassVelocity(particles)
      } yield {
        (p: Particle[V]) => p.copy(velocity = p.velocity - cmVelocity)
      }).getOrElse(
        (p: Particle[V]) => p
      )
    ))

    updateParticlesWithActions(Seq.concat(centerOfMassAction, integrationAlgorithm.init))
  }

  def next: M[FrameLog[V, Fig, M]] = {
    val actions = Seq.concat(
      integrationAlgorithm.iterationStep,
      Seq(UpdatePositions[V](p => limitConditions.positionLimitCondition(p.position)))
    )

    updateParticlesWithActions(actions)
  }

  protected def updateParticlesWithActions(actions: Seq[ParticlesChangeAction[V]]): M[FrameLog[V, Fig, M]] = {
    particlesReducer.applyChangeActions(particles)(actions)
      .map(newParticles => this.copy[V, Fig, M](particles = newParticles))
  }
}