package simulation.state

import cats.implicits._
import cats.{Monad, Parallel}
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState


case class ParticlesListState[V <: AlgebraicVector[V], Context[_] : Monad](
  override val counit: List[Particle[V]]
)(implicit P : Parallel[Context]) extends ParticlesState[V, Context, List] {

  override def getParticles: List[Particle[V]] = counit

  override def updateWithParticles(particles: List[Particle[V]]): Context[ParticlesState[V, Context, List]] = {
    Context.pure(this.copy[V, Context](counit = particles))
  }
}