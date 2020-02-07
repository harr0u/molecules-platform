package state

import domain.Particle
import domain.geometry.vector._


// TODO? Do I really need this trait and my specific abstraction over the molecules state
// what exactly should state class do?
//  map and map2 is interface to inject reducers Particles updating
//      map :: Particle => Particle
//      map2 :: (Particle, Particle) => (Particle, Particle) //// assuming state knows map2 should be called on every
//                                                           //// unique pair of Particles
//  particlesStream is interface to get all Inner particles regardless of implementation

// So it is abstraction over List, that could be much more complicated than flat list
// And should implement map, fold, map2 and other useful things
trait ParticlesState[V <: AlgebraicVector[V], F[_]] {
    def map(fn: (Particle[V]) => Particle[V]): F[ParticlesState[V, F]]

    def unit(particles: Seq[Particle[V]]): F[ParticlesState[V, F]]
    def counit: LazyList[Particle[V]]

    // Result particle should be updated first particle using second - !!dumb
    def particlesReduce(fn: (Particle[V], Particle[V]) => Particle[V]): F[ParticlesState[V, F]]
    // def map2(f: (Particle[V], Particle[V]) => (Particle[V], Particle[V])
    // TODO? abstract method ~map~ oslt to give reducer access to inner particles
}
