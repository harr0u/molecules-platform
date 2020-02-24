package calculation.numerical

import domain.geometry.vector.AlgebraicVector
import state.ParticlesChangeAction

trait TimeIntegrator[V <: AlgebraicVector[V]] {
  def init: Seq[ParticlesChangeAction[V]]
  def iterationStep: Seq[ParticlesChangeAction[V]]
}
