package molecules

import domain.Particle
import domain.vector._

class ParticlesSimpleSet[VectorType <: AlgebraicVector[VectorType]](
  val particles: Seq[Particle[VectorType]]
) extends ParticlesAbstractContainer[VectorType] {
  private val particlesNumber = particles.length
  def particlesStream: LazyList[Particle[VectorType]] = particles.to(LazyList)
  
  def particlePairsStream: LazyList[(Particle[VectorType], Particle[VectorType])] = {
    for {
      (particle1, i) <- particlesStream.zipWithIndex
      particle2 <- particlesStream.takeRight(particlesNumber - i - 1)
    } yield (particle1, particle2)
  }

  // I need some container box parametera
  // TODO: make class for box parameters
  def limitConditions(): Unit = { }
}