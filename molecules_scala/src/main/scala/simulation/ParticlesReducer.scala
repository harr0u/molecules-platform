package simulation

import cats.Monad
import domain.geometry.vector.AlgebraicVector
import actions.ParticlesChangeAction

trait ParticlesReducer[V <: AlgebraicVector[V], F[_], T[_]] {
  def applyChangeAction(particlesContainer: ParticlesState[V, F, T])(action: ParticlesChangeAction[V]): F[ParticlesState[V, F, T]]

  // Actions should be applied consistently
  def applyChangeActions(state: ParticlesState[V, F, T])(actions: Seq[ParticlesChangeAction[V]])
                        (implicit F : Monad[F]): F[ParticlesState[V, F, T]]
}
