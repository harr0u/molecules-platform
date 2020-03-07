package simulation.reducers

import cats.{Monad, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.{ParticlesReducer, ParticlesState}
import simulation.actions.{ParticleActionMap, ParticleActionReduce, ParticlesChangeAction}

class ParticlesStateReducer[V <: AlgebraicVector[V], F[_], T[_] : Traverse] extends ParticlesReducer[V, F, T] {

  override def applyChangeAction(state: ParticlesState[V, F, T])(action: ParticlesChangeAction[V]): F[ParticlesState[V, F, T]] = {
    action match {
      case map: ParticleActionMap[V] => state.mapParticles(map.mapFn)
      case reduce: ParticleActionReduce[V] => state.mapParticlesPairs { (p: Particle[V], other: ParticlesState[V, F, T]) =>
        other.getParticles.foldLeft(p)(reduce.reduceFn)
      }
    }
  }

  override def applyChangeActions(state: ParticlesState[V, F, T])(actions: Seq[ParticlesChangeAction[V]])
                                 (implicit F : Monad[F]): F[ParticlesState[V, F, T]] = {
    val squeezedActions: List[ParticlesChangeAction[V]] = actions.foldLeft(List[ParticlesChangeAction[V]]())((acc, act) => acc match {
      case ::(head, tail) => head match {
        case prevMap: ParticleActionMap[V] => act match {
          case nextMap: ParticleActionMap[V] => ((prevMap andThen nextMap) :: tail)
          case _ => act :: acc
        }
        case _ => act :: acc
      }
      case Nil => List(act)
    })

    squeezedActions.foldRight(F.pure(state))((action, newStateF) =>
      newStateF.flatMap(applyChangeAction(_)(action))
    )
  }
}