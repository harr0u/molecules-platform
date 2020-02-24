package calculation.physics

import calculation.limitConditions.SpaceConditions
import domain.Particle
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector

class LennardJonesPeriodicCutOffPotential[V <: AlgebraicVector[V], Fig <: GeometricFigure](
                                                                                            limitConditions: SpaceConditions[V, Fig],
                                                                                            cutOffDistance: Double
                                                                                          ) extends LennardJonesPeriodicPotential[V, Fig](limitConditions) {
  override def computeForceAndPotentialWithDistance(distance: V): (V, Double) = {
    if (distance.length > cutOffDistance) {
      (distance.zero, 0.0)
    } else {
      super.computeForceAndPotentialWithDistance(distance)
    }
  }
}
