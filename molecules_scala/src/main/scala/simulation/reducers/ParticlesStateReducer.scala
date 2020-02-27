package simulation.reducers

import cats.{Applicative, Monad}
import domain.geometry.vector.AlgebraicVector
import simulation.{ParticlesReducer, ParticlesState}
import simulation.actions.{ParticleActionMap, ParticleActionReduce, ParticlesChangeAction}

class ParticlesStateReducer[V <: AlgebraicVector[V], F[_]] extends ParticlesReducer[V, F] {

  override def applyChangeAction(state: ParticlesState[V, F])(action: ParticlesChangeAction[V]): F[ParticlesState[V, F]] = {
    action match {
      case map: ParticleActionMap[V] => state.map(map.mapFn)
      case reduce: ParticleActionReduce[V] => state.reduce(reduce.reduceFn)
    }
  }

  override def applyChangeActions(state: ParticlesState[V, F])(actions: Seq[ParticlesChangeAction[V]])
                                 (implicit F : Monad[F]): F[ParticlesState[V, F]] = {
    // In case of empty list we should get F[State] type, maybe make method to zip State to F[State],
    // Like unit for container of container
    // Make F - F <: Functor?
    // If it works, remove Futures and loose dependency from ParticlesState[V, Future] to ParticlesState[V, F]
    val squeezedActions: List[ParticlesChangeAction[V]] = actions.foldLeft(List[ParticlesChangeAction[V]]())((acc, act) => acc match {
      case ::(head, tail) => head match {
        case prevMap: ParticleActionMap[V] => act match {
          case nextMap: ParticleActionMap[V] => ((prevMap andThen nextMap) :: tail)
          case _ => act :: acc
        }
        case _ => act :: acc
      }
      case Nil => List(act)
    } )

    squeezedActions.foldRight(F.pure(state))((action, newStateF) =>
      F.flatMap(newStateF)((newState) => applyChangeAction(newState)(action))
    )
  }
}