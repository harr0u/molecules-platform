package simulation

import cats.{Monad, Parallel}
import cats.implicits._
import domain.Particle
import domain.geometry.vector._
import molecules.Utils._

// TODO? Do I really need this trait and my specific abstraction over the molecules state
// what exactly should state class do?
//  map and map2 is interface to inject reducer's Particles updating
//      map :: Particle => Particle
//      map2 :: (Particle, Particle) => (Particle, Particle) //// assuming state knows map2 should be called on every
//                                                           //// unique pair of Particles

// So it is abstraction over List, that could be much more complicated than flat list
// And should implement map, fold, map2 and other useful things
abstract class ParticlesState[V <: AlgebraicVector[V], F[_] : Monad]{
    val F: Monad[F] = implicitly[Monad[F]]

    def getParticles: List[Particle[V]]
    def updateWithParticles(particles: Seq[Particle[V]]): F[ParticlesState[V, F]]

    def mapParticles(mapFn: (Particle[V]) => Particle[V]): F[ParticlesState[V, F]] = {
      this map mapFn flatMap updateWithParticles
    }

    def mapParticlesPairs(mapFn: Particle[V] => ParticlesState[V, F] => Particle[V]): F[ParticlesState[V, F]] = {
      val liftedMapFn: Particle[V] => ParticlesState[V, F] => F[Particle[V]] = (p) => (s) => F.pure(mapFn(p)(s))
      val getRestParticles = (_: Particle[V]) => F.pure(this)

      mapWithState(getRestParticles)(liftedMapFn) flatMap updateWithParticles
    }

    def map[B](f: Particle[V] => B): F[List[B]] = {
      getParticles
        .parTraverse(p => F.pure(f(p)))
    }

    def mapWithState[B](makeState: Particle[V] => F[ParticlesState[V, F]])
                       (mapFn: Particle[V] => ParticlesState[V, F] => F[B]): F[List[B]] = {
      getParticles
        .parTraverse(particle => makeState(particle).flatMap(mapFn(particle)))
    }
}
