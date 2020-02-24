package calculation.physics
import calculation.limitConditions.SpaceConditions
import domain.Particle
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector

abstract trait PeriodicPotential[V <: AlgebraicVector[V], F <: GeometricFigure] extends LennardJonesPotential[V] {
  def limitConditions: SpaceConditions[V, F]
  abstract override def computeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double) = {
    super.computeForceAndPotentialWithDistance(limitConditions.getDistanceBetween(particle1.position, particle2.position))
  }
}
