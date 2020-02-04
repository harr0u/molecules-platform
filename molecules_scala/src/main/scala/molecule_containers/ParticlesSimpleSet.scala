package molecules

import domain.Particle
import domain.vector._

class ParticlesSimpleSet[V <: AlgebraicVector[V]](
  val particles: Seq[Particle[V]]
) extends ParticlesAbstractContainer[V] {
  private val particlesNumber = particles.length

  def particlesStream: LazyList[Particle[V]] = particles.to(LazyList)
  
  def particlePairsStream: LazyList[(Particle[V], Particle[V])] = {
    for {
      (particle1, i) <- particlesStream.zipWithIndex
      particle2 <- particlesStream.takeRight(particlesNumber - i - 1)
    } yield (particle1, particle2)
  }

  def applyChangeAction(action: ParticlesChangeAction[V]): ParticlesAbstractContainer[V] = {
    new ParticlesSimpleSet[V](applyParticlesActionForSeq(particles, action))
  }

  def applyChangeActions(actions: Seq[ParticlesChangeAction[V]]): ParticlesAbstractContainer[V] = {
    actions.foldLeft(this: ParticlesAbstractContainer[V])(
      (acc, action) => acc.applyChangeAction(action)
    )
  }

  protected def applyParticlesActionForSeq(seq: Seq[Particle[V]], action: ParticlesChangeAction[V]): Seq[Particle[V]] = {
    action match {
      case act: ZeroForces[V] => this.applyZeroForcesActionForSeq(particles, act)
      case act: ZeroPotentials[V] => this.applyZeroPotentialsActionForSeq(particles, act)
      case act: UpdatePositions[V] => this.applyUpdatePositionsActionForSeq(particles, act)
      case act: UpdateVelocities[V] => this.applyUpdateVelocitiesActionForSeq(particles, act)
      case act: UpdateForceAndPotential[V] => this.applyUpdateForceAndPotentialActionForSeq(particles, act)
    }
  }

  protected def applyZeroForcesActionForSeq(seq: Seq[Particle[V]], action: ZeroForces[V]): Seq[Particle[V]] = {
    seq.map((p) => p.copy(force = p.force.zero))
  }

  protected def applyZeroPotentialsActionForSeq(seq: Seq[Particle[V]], action: ZeroPotentials[V]): Seq[Particle[V]] = {
    seq.map((p) => p.copy(potential = 0.0))
  }

  protected def applyUpdatePositionsActionForSeq(seq: Seq[Particle[V]], action: UpdatePositions[V]): Seq[Particle[V]] = {
    seq.map((p) => p.copy(position = action.fn(p)))
  }

  protected def applyUpdateVelocitiesActionForSeq(seq: Seq[Particle[V]], action: UpdateVelocities[V]): Seq[Particle[V]] = {
    seq.map((p) => p.copy(velocity = action.fn(p)))
  }

  protected def applyUpdateForceAndPotentialActionForSeq(seq: Seq[Particle[V]], action: UpdateForceAndPotential[V]): Seq[Particle[V]] = {
    this.particlePairsStream.foldLeft(Map[Int, Particle[V]]())((acc: Map[Int, Particle[V]], particlesPair) => {
        val (particle1, particle2) = particlesPair
        val (newParticle1, newParticle2) = action.fn(particle1, particle2)

        val mergeParticle = (acc: Map[Int, Particle[V]], newParticle: Particle[V]) => {
          val mergedParticle: Particle[V] = acc
            .get(newParticle.id)
            .map((p) => p.copy(
              force = p.force + newParticle.force,
              potential = p.potential + newParticle.potential
            ))
            .getOrElse(newParticle)

          acc + (mergedParticle.id -> mergedParticle)
        }

        mergeParticle(mergeParticle(acc, newParticle1), newParticle2)
      }).values.toSeq
    }
}