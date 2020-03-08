package simulation

import cats.Monad
import domain.geometry.vector.AlgebraicVector
import actions.ParticlesChangeAction

trait ParticlesReducer[V <: AlgebraicVector[V], Context[_], T[_]] {
  def applyChangeAction(state: ParticlesState[V, Context, T])
                       (action: Context[ParticlesChangeAction[V]])
                       (implicit Context : Monad[Context]): Context[ParticlesState[V, Context, T]]

  // Actions should be applied consistently
  def applyChangeActions(state: ParticlesState[V, Context, T])
                        (actions: Context[Seq[ParticlesChangeAction[V]]])
                        (implicit Context : Monad[Context]): Context[ParticlesState[V, Context, T]]
}
