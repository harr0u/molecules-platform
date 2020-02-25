package simulation.state

import cats.{Applicative, Traverse}
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState

import scala.concurrent.Future

case class ParticlesListState[V <: AlgebraicVector[V], F[_]](
  particles: List[Particle[V]]
)(implicit F : Applicative[F], SeqForF : Traverse[List]) extends ParticlesState[V, F] {

  override def getParticles: List[Particle[V]] = particles

  override def map(fn: Particle[V] => Particle[V]): F[ParticlesState[V, F]] = F.pure(ParticlesListState[V, F](particles.map(fn)))

  override def reduce(fn: (Particle[V], Particle[V]) => Particle[V]): F[ParticlesState[V, F]] = {
    val newParticlesF: F[List[Particle[V]]] = SeqForF.sequence(particles.zipWithIndex.map {
      case (particle, i) => F.pure {
        particles.zipWithIndex.foldLeft(particle)((accParticle, pi) => {
          val (secondParticle, j) = pi

          if (i != j) fn(accParticle, secondParticle) else accParticle
        })
      }
    })
    F.map(newParticlesF)((newParticles: List[Particle[V]]) => this.copy(particles = newParticles))
  }
}
