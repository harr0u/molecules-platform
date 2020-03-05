package simulation.state

import cats.{Applicative, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState
import molecules.Utils._


case class ParticlesListState[V <: AlgebraicVector[V], F[_]](
  particles: List[Particle[V]]
)(implicit F : Applicative[F], SeqForF : Traverse[List]) extends ParticlesState[V, F] {

  override def getParticles: List[Particle[V]] = particles

  override def reduce(fn: (Particle[V], Particle[V]) => Particle[V]): F[ParticlesState[V, F]] = {
    val reduceWithoutCollision = (accParticle: Particle[V], otherParticle: Particle[V]) => {
      if (accParticle.id != otherParticle.id) {
        fn(accParticle, otherParticle)
      } else {
        accParticle
      }
    }

    particles
      .map(p => particles.foldLeft(p)(reduceWithoutCollision) |> F.pure[Particle[V]])
      .sequence
      .map((newParticles: List[Particle[V]]) => this.copy(particles = newParticles))
  }
}
