package calculation.numerical

import domain.geometry.figures.GeometricFigure
import calculation.limit_conditions.LimitConditions
import calculation.physics.PotentialCalculator
import domain.Particle
import domain.geometry.vector._
import state.{ParticleReducer, ParticlesState, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


case class LeapFrogIteration[V <: AlgebraicVector[V], Fig <: GeometricFigure](
  particles: ParticlesState[V, Future],
  particlesReducer: ParticleReducer[V, Future],
  potentialCalculator: PotentialCalculator[V],
  limitConditions: LimitConditions[V, Fig],
  deltaStepTime: Double = 0.0001
) {
  def init(): Future[LeapFrogIteration[V, Fig]] = {
    for {
      newParticles <- particlesReducer.applyChangeAction(
        particles,
        UpdateForceAndPotential[V](recomputeForceAndPotential)
      )
    } yield {
      this.copy[V, Fig](particles = newParticles)
    }
  }

  private val deltaStepTimeSquared: Double = deltaStepTime * deltaStepTime
  def iterationStep(): Future[LeapFrogIteration[V, Fig]] = {
    val newParticlesFuture: Future[ParticlesState[V, Future]] = particlesReducer.applyChangeActions(particles, List(
      UpdatePositions((p) => p.position + p.velocity * deltaStepTime + p.acceleration * (deltaStepTimeSquared / 2)),
      UpdatePositions((p) => limitConditions.positionLimitCondition(p.position)),
      UpdateVelocities((p) => p.velocity + p.acceleration * (deltaStepTime / 2)),
      ZeroForces(),
      ZeroPotentials(),
      UpdateForceAndPotential(recomputeForceAndPotential),
      UpdateVelocities((p) => p.velocity + p.acceleration * (deltaStepTime / 2)),
    ));

    for {
      newParticles <- newParticlesFuture
    } yield {
      this.copy[V, Fig](particles = newParticles)
    }
  }

  private def recomputeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (Particle[V], Particle[V]) = {
    val (force, potential: Double) = potentialCalculator.computeForceAndPotential(
      limitConditions.distanceLimitCondition(particle2.position - particle1.position)
    )

    val updatedParticle1 = particle1.copy(force = particle1.force + force, potential = particle1.potential + potential)
    val updatedParticle2 = particle2.copy(force = particle2.force - force, potential = particle2.potential + potential)

    (updatedParticle1, updatedParticle2)
  }
}