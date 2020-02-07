package state

import cats.Id
import domain.Particle
import domain.geometry.vector._

case class ParticlesSeqState[V <: AlgebraicVector[V]](
  particles: Seq[Particle[V]]
) extends ParticlesState[V, Id] {

  override def counit: LazyList[Particle[V]] = {
    particles.to(LazyList)
  }

  override def unit(particles: Seq[Particle[V]]): ParticlesState[V, Id] = {
    ParticlesSeqState[V](particles)
  }

  override def map(fn: Particle[V] => Particle[V]): ParticlesState[V, Id] = {
    this.unit(particles.map(fn))
  }

  override def particlesReduce(fn: (Particle[V], Particle[V]) => Particle[V]): ParticlesState[V, Id] = {
    this.unit(particles.zipWithIndex.map { case (particle, i) => {
      particles.zipWithIndex.foldLeft(particle)((accParticle, pi) => {
        val (secondParticle, j) = pi

        if (i != j) fn(accParticle, secondParticle) else accParticle
      })
    }})
  }
}
