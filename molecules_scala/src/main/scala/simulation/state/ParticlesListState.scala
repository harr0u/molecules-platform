package simulation.state

import cats.{Applicative, Monad, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState
import molecules.Utils._


case class ParticlesListState[V <: AlgebraicVector[V], Context[_]](
  particles: List[Particle[V]]
)(implicit Context : Monad[Context]) extends ParticlesState[V, Context, List] {
  override def getParticles: List[Particle[V]] = particles

  override def updateWithParticles(particles: List[Particle[V]]): Context[ParticlesState[V, Context, List]] = {
    Context.pure(this.copy[V, Context](particles = particles))
  }

  override def map[B](f: Particle[V] => B): Context[List[B]] = {
    getParticles
      .parTraverse(p => Context.pure(f(p)))
  }

  override def mapWithState[B](makeState: Particle[V] => Context[ParticlesState[V, Context, List]])
                     (mapFn: Particle[V] => ParticlesState[V, Context, List] => Context[B]): Context[List[B]] = {
    getParticles
      .parTraverse(particle => makeState(particle)
        .flatMap(mapFn(particle)))
  }
}
