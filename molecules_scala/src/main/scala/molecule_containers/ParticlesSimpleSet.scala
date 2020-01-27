package molecules

import domain.Particle
import domain.vector._
import molecules.ParticlesAbstractContainer

class ParticlesSimpleSet[VectorType <: AlgebraicVector[VectorType]](
  val particles: Seq[Particle[VectorType]]
) extends ParticlesAbstractContainer[VectorType] {
  def particlesStream: LazyList[Particle[VectorType]] = particles.to(LazyList)
  
  def particlePairsStream: LazyList[(Particle[VectorType], Particle[VectorType])] = {
    for {
      (particle1, i) <- particles.zipWithIndex.to(LazyList)
      particle2 <- particles.takeRight(particles.length - i - 1).to(LazyList)
    } yield (particle1, particle2)
  }

  // I need some container box parametera
  // TODO: make class for box parameters
  def limitConditions(): Unit = { }
}