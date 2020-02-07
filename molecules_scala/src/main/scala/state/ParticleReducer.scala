package state

import domain.geometry.vector.AlgebraicVector


trait ParticleReducer[V <: AlgebraicVector[V]] {
  def applyChangeAction(particlesContainer: ParticlesState[V], action: ParticlesChangeAction[V]): ParticlesState[V]
  // Actions should be applied consistently
  def applyChangeActions(particlesContainer: ParticlesState[V], actions: Seq[ParticlesChangeAction[V]]): ParticlesState[V]
}