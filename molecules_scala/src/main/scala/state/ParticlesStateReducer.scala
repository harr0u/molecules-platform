package state

import cats.{Functor, Monad}
import domain.Particle
import domain.geometry.vector.AlgebraicVector
import scala.concurrent.ExecutionContext.Implicits.global
import state.{ParticleReducer, ParticlesChangeAction, ParticleActionMap, ParticleActionReduce, UpdateVelocities, ZeroForces, ZeroPotentials}

import scala.concurrent.Future

class ParticlesStateReducer[V <: AlgebraicVector[V]] extends ParticleReducer[V, Future] {

  override def applyChangeAction(state: ParticlesState[V, Future], action: ParticlesChangeAction[V]): Future[ParticlesState[V, Future]] = {
    action match {
      case map: ParticleActionMap[V] => state.map(map.mapFn)
      case reduce: ParticleActionReduce[V] => state.reduce(reduce.reduceFn)
    }
  }

  override def applyChangeActions(state: ParticlesState[V, Future], actions: Seq[ParticlesChangeAction[V]]): Future[ParticlesState[V, Future]] = {
    // In case of empty list we should get F[State] type, maybe make method to zip State to F[State],
    // Like unit for container of container
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

    squeezedActions.foldRight(state.unit())((action, newState) =>
      newState.flatMap((state) => applyChangeAction(state, action))
    )
  }
}