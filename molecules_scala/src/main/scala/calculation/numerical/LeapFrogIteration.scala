package calculation.numerical

import domain.geometry.figures.GeometricFigure
import calculation.limitConditions.SpaceConditions
import calculation.physics.{CenterOfMassCalculator, LennardJonesPotential, PotentialCalculator}
import domain.Particle
import domain.geometry.vector._
import state.{ParticleActionMap, ParticleReducer, ParticlesChangeAction, ParticlesState, ParticlesStateReducer, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


// By the way, it looks strange: Leap From Integration algorithm + iterator pattern, can I cut off algo from progrmng?
case class LeapFrogIteration[V <: AlgebraicVector[V], Fig <: GeometricFigure](
                                                                               particles: ParticlesState[V, Future],
                                                                               limitConditions: SpaceConditions[V, Fig],
                                                                               particlesReducer: ParticleReducer[V, Future] = new ParticlesStateReducer[V],
                                                                               `∆t`: Double = 0.0001
                                                                             )(implicit potentialCalculator: PotentialCalculator[V]) {
  def init(): Future[LeapFrogIteration[V, Fig]] = {
    val centerOfMassAction: ParticleActionMap[V] = new ParticleActionMap[V](
      (for {
        cmVelocity <- CenterOfMassCalculator.findCenterMassVelocity(particles)
      } yield {
        (p: Particle[V]) => p.copy(velocity = p.velocity - cmVelocity)
      }).getOrElse(
        (p: Particle[V]) => p
      )
    )

    for {
      newParticles <- particlesReducer.applyChangeActions(particles, List(
        centerOfMassAction,
        ZeroForces(),
        ZeroPotentials(),
        UpdateForceAndPotential[V](recomputeForceAndPotential)
      ))
    } yield {
      this.copy[V, Fig](particles = newParticles)
    }
  }

  private val `∆t∆t`: Double = `∆t` * `∆t`
  def iterationStep(): Future[LeapFrogIteration[V, Fig]] = {
    val newParticlesFuture: Future[ParticlesState[V, Future]] = particlesReducer.applyChangeActions(particles, List(
      UpdatePositions((p) => p.position + p.velocity * `∆t` + p.acceleration * (`∆t∆t` / 2)),
      UpdatePositions((p) => limitConditions.positionLimitCondition(p.position)),
      UpdateVelocities((p) => p.velocity + p.acceleration * (`∆t` / 2)),
      ZeroForces(),
      ZeroPotentials(),
      UpdateForceAndPotential(recomputeForceAndPotential),
      UpdateVelocities((p) => p.velocity + p.acceleration * (`∆t` / 2)),
    ));

    for {
      newParticles <- newParticlesFuture
    } yield {
      this.copy[V, Fig](particles = newParticles)
    }
  }

  private def recomputeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double) = {
    potentialCalculator.computeForceAndPotential(
      limitConditions.getDistanceBetween(particle1.position, particle2.position)
    )
  }
}