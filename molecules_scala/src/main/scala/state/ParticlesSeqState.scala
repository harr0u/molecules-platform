package state

import cats.Id
import domain.Particle
import domain.geometry.vector._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class ParticlesSeqState[V <: AlgebraicVector[V]](
  particles: Seq[Particle[V]]
) extends ParticlesState[V, Future] {

  override def counit: LazyList[Particle[V]] = {
    particles.to(LazyList)
  }

  override def unit(): Future[ParticlesState[V, Future]] = {
    Future.successful(ParticlesSeqState[V](particles))
  }

  override def map(fn: Particle[V] => Particle[V]): Future[ParticlesState[V, Future]] = {
    Future.successful(ParticlesSeqState[V](particles.map(fn)))
  }

  override def reduce(fn: (Particle[V], Particle[V]) => Particle[V]): Future[ParticlesState[V, Future]] = {
    Future.sequence(particles.zipWithIndex.map {
      case (particle, i) => Future {
        particles.zipWithIndex.foldLeft(particle)((accParticle, pi) => {
          val (secondParticle, j) = pi

          if (i != j) fn(accParticle, secondParticle) else accParticle
        })
      }
    }).map((newParticles) => this.copy(particles = newParticles))
  }
}
