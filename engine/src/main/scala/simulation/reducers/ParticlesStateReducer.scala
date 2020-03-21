package simulation.reducers

import cats.{Monad, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import simulation.{ParticlesReducer, ParticlesState}
import simulation.actions.{ParticleActionMap, ParticleActionReduce, ParticlesChangeAction}

class ParticlesStateReducer[V <: AlgebraicVector[V], C[_], T[_] : Traverse] extends ParticlesReducer[V, C, T] {

  override def applyChangeAction(state: ParticlesState[V, C, T])
                                (action: C[ParticlesChangeAction[V]])
                                (implicit C : Monad[C]): C[ParticlesState[V, C, T]] = {
    action.flatMap {
      case map: ParticleActionMap[V] => state.mapParticles(map.mapFn)
      case reduce: ParticleActionReduce[V] => state.mapParticlesWithState {
        (p: Particle[V], other: ParticlesState[V, C, T]) => other.getParticles.foldLeft(p)(reduce.reduceFn)
      }
    }
  }

  override def applyChangeActions(state: ParticlesState[V, C, T])
                                 (actions: C[Seq[ParticlesChangeAction[V]]])
                                 (implicit C : Monad[C]): C[ParticlesState[V, C, T]] = {
    val squeezedActions: C[List[ParticlesChangeAction[V]]] = actions.map{
      _.foldLeft(List[ParticlesChangeAction[V]]())((acc, act) => acc match {
        case ::(head, tail) => head match {
          case prevMap: ParticleActionMap[V] => act match {
            case nextMap: ParticleActionMap[V] => ((prevMap andThen nextMap) :: tail)
            case _ => act :: acc
          }
          case _ => act :: acc
        }
        case Nil => List(act)
      })
    }

    squeezedActions.flatMap(_.foldRight(C.pure(state))((act, stateContext : C[ParticlesState[V, C, T]]) =>
      stateContext.flatMap(applyChangeAction(_)(C.pure(act)))
    ))
  }
}