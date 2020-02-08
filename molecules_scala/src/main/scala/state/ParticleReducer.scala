package state

import domain.geometry.vector.AlgebraicVector


trait ParticleReducer[V <: AlgebraicVector[V], F[_]] {
  def applyChangeAction(particlesContainer: ParticlesState[V, F], action: ParticlesChangeAction[V]): F[ParticlesState[V, F]]
  // Actions should be applied consistently
  def applyChangeActions(particlesContainer: ParticlesState[V, F], actions: Seq[ParticlesChangeAction[V]]): F[ParticlesState[V, F]]
}