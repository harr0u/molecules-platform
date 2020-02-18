package calculation.numerical

import domain.geometry.figures.GeometricFigure
import calculation.limitConditions.SpaceConditions
import calculation.physics.{CenterOfMassCalculator, PotentialCalculator}
import domain.Particle
import domain.geometry.vector._
import state.{ParticleActionMap, ParticleReducer, ParticlesChangeAction, ParticlesState, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


case class LeapFrogIteration[V <: AlgebraicVector[V], Fig <: GeometricFigure](
                                                                               particles: ParticlesState[V, Future],
                                                                               particlesReducer: ParticleReducer[V, Future],
                                                                               potentialCalculator: PotentialCalculator[V],
                                                                               limitConditions: SpaceConditions[V, Fig],
                                                                               `∆t`: Double = 0.0001
) {
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

  private val deltaStepTimeSquared: Double = `∆t` * `∆t`
  def iterationStep(): Future[LeapFrogIteration[V, Fig]] = {
    val newParticlesFuture: Future[ParticlesState[V, Future]] = particlesReducer.applyChangeActions(particles, List(
      UpdatePositions((p) => p.position + p.velocity * `∆t` + p.acceleration * (deltaStepTimeSquared / 2)),
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