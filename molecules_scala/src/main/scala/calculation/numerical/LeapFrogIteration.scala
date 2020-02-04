package calculation.numerical

import calculation.geometry.figures.GeometricFigure
import calculation.limit_conditions.LimitConditions
import calculation.physics.PotentialCalculator
import domain.vector._
import molecules.ParticlesAbstractContainer


class LeapFrogIteration[VectorType <: AlgebraicVector[VectorType], BoundariesType <: GeometricFigure](
                                                                    val particles: ParticlesAbstractContainer[VectorType],
                                                                    val potentialCalculator: PotentialCalculator[VectorType],
                                                                    val limitConditions: LimitConditions[VectorType, BoundariesType],
                                                                    val deltaStepTime: Double = 0.0001
) extends IterationStepPerformer {
  def init(): Unit = {
    this.entirelyRecomputeForcesAndPotentials()
  }

  private val deltaStepTimeSquared: Double = deltaStepTime * deltaStepTime
  def iterationStep(): Unit = {

    for (particle <- particles.particlesStream) {
        particle.position = particle.position + particle.velocity * deltaStepTime + particle.acceleration * (deltaStepTimeSquared / 2);
        particle.velocity = particle.velocity + particle.acceleration * (deltaStepTime / 2);
    }

    particles.particlesStream.foreach((p) => limitConditions.positionLimitCondition(p.position))

    this.entirelyRecomputeForcesAndPotentials()

    for (particle <- particles.particlesStream) {
      particle.velocity = particle.velocity + particle.acceleration * (deltaStepTime / 2)
    }
  }

  private def entirelyRecomputeForcesAndPotentials(): Unit = {
    particles.particlesStream.foreach(_.clearForceAndPotential())

    for ((particle1, particle2) <- particles.particlePairsStream) {
      val (force, potential: Double) = potentialCalculator.computeForceAndPotential(
        limitConditions.distanceLimitCondition(particle2.position - particle1.position)
      )

      particle1.force = particle1.force + force
      particle2.force = particle2.force - force

      particle1.potential = particle1.potential + potential
      particle2.potential = particle2.potential + potential
    }
  }

  def dispose(): Unit = {}
}