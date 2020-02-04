package calculation.numerical

import calculation.geometry.figures.GeometricFigure
import calculation.limit_conditions.LimitConditions
import calculation.physics.PotentialCalculator
import domain.Particle
import domain.vector._
import molecules.{ParticlesAbstractContainer, UpdateForceAndPotential, UpdatePositions, UpdateVelocities, ZeroForces, ZeroPotentials}


class LeapFrogIteration[V <: AlgebraicVector[V], BoundariesType <: GeometricFigure](
                                                                    val particles: ParticlesAbstractContainer[V],
                                                                    val potentialCalculator: PotentialCalculator[V],
                                                                    val limitConditions: LimitConditions[V, BoundariesType],
                                                                    val deltaStepTime: Double = 0.0001
) {
  def init(): LeapFrogIteration[V, BoundariesType] = {
    val newParticles = particles.applyChangeAction(UpdateForceAndPotential(recomputeForceAndPotential))
    this.updateParticles(newParticles)
  }

  private val deltaStepTimeSquared: Double = deltaStepTime * deltaStepTime
  def iterationStep(): LeapFrogIteration[V, BoundariesType] = {
    val newParticles = particles.applyChangeActions(List(
      UpdatePositions((p) => p.position + p.velocity * deltaStepTime + p.acceleration * (deltaStepTimeSquared / 2)),
      UpdatePositions((p) => limitConditions.positionLimitCondition(p.position)),
      UpdateVelocities((p) => p.velocity + p.acceleration * (deltaStepTime / 2)),
      ZeroForces(),
      ZeroPotentials(),
      UpdateForceAndPotential(recomputeForceAndPotential),
      UpdateVelocities((p) => p.velocity + p.acceleration * (deltaStepTime / 2)),
    ));

    this.updateParticles(newParticles)
  }

  private def recomputeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (Particle[V], Particle[V]) = {
    val (force, potential: Double) = potentialCalculator.computeForceAndPotential(
      limitConditions.distanceLimitCondition(particle2.position - particle1.position)
    )

    val updatedParticle1 = particle1.copy(force = particle1.force + force, potential = particle1.potential + potential)
    val updatedParticle2 = particle2.copy(force = particle2.force - force, potential = particle2.potential + potential)

    (updatedParticle1, updatedParticle2)
  }

  private def updateParticles(newParticles: ParticlesAbstractContainer[V]): LeapFrogIteration[V, BoundariesType] = {
    new LeapFrogIteration[V, BoundariesType](
      newParticles,
      this.potentialCalculator,
      this.limitConditions,
      this.deltaStepTime
    )
  }
}