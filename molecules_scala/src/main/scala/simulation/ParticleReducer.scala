package simulation

import cats.Monad
import domain.geometry.vector.AlgebraicVector
import actions.ParticlesChangeAction

trait ParticleReducer[V <: AlgebraicVector[V], F[_]] {
  def applyChangeAction(particlesContainer: ParticlesState[V, F])(action: ParticlesChangeAction[V]): F[ParticlesState[V, F]]
  // Actions should be applied consistently
  def applyChangeActions(state: ParticlesState[V, F])(actions: Seq[ParticlesChangeAction[V]])
                        (implicit F : Monad[F]): F[ParticlesState[V, F]]
}
