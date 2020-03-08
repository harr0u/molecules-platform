package simulation

import cats.{Monad, Parallel, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.{AlgebraicVector, _}
import molecules.Utils._

// TODO? Do I really need this trait and my specific abstraction over the molecules state
// what exactly should state class do?
//  map and map2 is interface to inject reducer's Particles updating
//      map :: Particle => Particle
//      map2 :: (Particle, Particle) => (Particle, Particle) //// assuming state knows map2 should be called on every
//                                                           //// unique pair of Particles

// So it is abstraction over T, that could be much more complicated than T
// And should implement map, fold, map2 and other useful things
abstract class ParticlesState[V <: AlgebraicVector[V], Context[_] : Monad, T[_] : Traverse]
(implicit P : Parallel[Context]){
  def getParticles: Seq[Particle[V]]
  def counit: T[Particle[V]]

  def updateWithParticles(particles: T[Particle[V]]): Context[ParticlesState[V, Context, T]]

  def mapParticles(mapFn: (Particle[V]) => Particle[V]): Context[ParticlesState[V, Context, T]] = {
    map(mapFn)
      .flatMap(updateWithParticles)
  }

  def mapParticlesPairs(mapFn: (Particle[V], ParticlesState[V, Context, T]) => Particle[V]): Context[ParticlesState[V, Context, T]] = {
    mapWithState(_ => Context.pure(this))(p => s => Context.pure(mapFn(p, s)))
      .flatMap(updateWithParticles)
  }

  def map[B](f: Particle[V] => B): Context[T[B]] = {
    counit
      .parTraverse(p => Context.pure(f(p)))
  }

  def mapWithState[B](makeState: Particle[V] => Context[ParticlesState[V, Context, T]])
                     (mapFn: Particle[V] => ParticlesState[V, Context, T] => Context[B]): Context[T[B]] = {
    counit
      .parTraverse(particle => makeState(particle)
        .flatMap(mapFn(particle)))
  }

  lazy val Context: Monad[Context] = implicitly[Monad[Context]]
}
