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
    new ParticlesSimpleSet[V](action match {
      case ZeroForces() => particles.map((p) => p.copy(force = p.force.zero))
      case ZeroPotentials() => particles.map((p) => p.copy(potential = 0.0))
      case UpdatePositions(fn) => particles.map((p) => p.copy(position = fn(p)))
      case UpdateVelocities(fn) => particles.map((p) => p.copy(velocity = fn(p)))
      case UpdateForceAndPotential(fn) => {
        this.particlePairsStream.foldLeft(Map[Int, Particle[V]]())((acc: Map[Int, Particle[V]], particlesPair) => {
          val (particle1, particle2) = particlesPair
          val (newParticle1, newParticle2) = fn(particle1, particle2)

          val mergeParticle = (acc: Map[Int, Particle[V]], newParticle: Particle[V]) => {
            val mergedParticle: Particle[V] = acc.get(newParticle.id).map((p) => p.copy(
              force = p.force + newParticle.force,
              potential = p.potential + newParticle.potential
            )).getOrElse(newParticle)

            acc + (mergedParticle.id -> mergedParticle)
          }

          mergeParticle(mergeParticle(acc, newParticle1), newParticle2)
        }).values.toSeq
      }
    })
  }

  def applyChangeActions(actions: Seq[ParticlesChangeAction[V]]): ParticlesAbstractContainer[V] = {
    actions.foldLeft(this: ParticlesAbstractContainer[V])(
      (acc, action) => acc.applyChangeAction(action)
    )
  }
}