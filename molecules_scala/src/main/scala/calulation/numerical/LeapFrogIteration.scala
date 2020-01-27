package calculation.numerical

import calculation.physics.PotentialCalculator
import domain.Particle
import domain.vector._
import molecules.ParticlesAbstractContainer


class LeapFrogIteration[VectorType <: AlgebraicVector[VectorType]](
                                                                    val particles: ParticlesAbstractContainer[VectorType],
                                                                    val potentialCalculator: PotentialCalculator[VectorType],
                                                                    val deltaStepTime: Double = 0.0001
) extends IterationStepPerformer {
  def init(): Unit = {
    entirelyRecomputeForcesAndPotentials()
  }

  private val deltaStepTimeSquared: Double = deltaStepTime * deltaStepTime
  def iterationStep(): Unit = {
    for (particle: Particle[VectorType] <- particles.particlesStream) {
        particle.position = particle.position + particle.velocity * deltaStepTime + particle.acceleration * (deltaStepTimeSquared / 2);
        particle.velocity = particle.velocity + particle.acceleration * (deltaStepTime / 2);
    }
    // TODO?: limit conditions in iterator, limit conditions in potential computing...
    // TODO?: make limit conditions part of box class?
    // TODO?: limit condition for force vector and limit condition after molecule translating are different? :)
    particles.limitConditions()

    entirelyRecomputeForcesAndPotentials()

    for (particle <- particles.particlesStream) {
      particle.velocity = particle.velocity + particle.acceleration * (deltaStepTime / 2)
    }
  }

  private def entirelyRecomputeForcesAndPotentials(): Unit = {
    particles.particlesStream.foreach(_.clearForceAndPotential())

    for ((particle1, particle2) <- particles.particlePairsStream) {
      val (force: VectorType, potential: Double) = potentialCalculator.computeForceAndPotential(
        particle1.position,
        particle2.position
      )

      particle1.force = particle1.force + force
      particle2.force = particle2.force - force

      particle1.potential = particle1.potential + potential
      particle2.potential = particle2.potential + potential
    }
  }

  def dispose(): Unit = {}
}