package calculation.physics.potentials

import calculation.space.SpaceConditions
import domain.Particle
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector._

class LennardJonesPotential[V <: AlgebraicVector[V]] extends PairwisePotentialCalculator[V] {
  def computeForceAndPotential(particle1: Particle[V], particle2: Particle[V]): (V, Double) = {
    computeForceAndPotentialWithDistance(particle2.position - particle1.position)
  }

  def computeForceAndPotentialWithDistance(distance: V): (V, Double) = {
    val distanceSquaredInverted: Double = 1.0 / distance.squaredLength
    val r6: Double = Math.pow(distanceSquaredInverted, 3)
    val r12: Double = r6 * r6

    // Read More: https://en.wikipedia.org/wiki/Lennard-Jones_potential#Dimensionless_(reduced)_units
    val force: V = distance * (-48.0 * (r12 * distanceSquaredInverted - 0.5 * r6 * distanceSquaredInverted))
    val potential: Double = 2 * (r12 - r6)

    (force, potential)
  }
}

// LennardJonesPotential.factory.withPeriodic().with


object LennardJonesPotential {
  def potential[V <: AlgebraicVector[V]]: LennardJonesPotential[V] = new LennardJonesPotential[V]

  def periodicPotential[V <: AlgebraicVector[V], Fig <: GeometricFigure](spaceConditions: SpaceConditions[V, Fig]): LennardJonesPotential[V] = {
    new LennardJonesPotential[V]
      with PeriodicPotential[V, Fig] {
      override def limitConditions: SpaceConditions[V, Fig] = spaceConditions
    }
  }

  def periodicCutOffPotential[V <: AlgebraicVector[V], Fig <: GeometricFigure](spaceConditions: SpaceConditions[V, Fig])(cutOff: Double = 5.0): LennardJonesPotential[V] = {
    new LennardJonesPotential[V]
      with PeriodicPotential[V, Fig]
      with CutOffPotentialOptimization[V] {
      override def limitConditions: SpaceConditions[V, Fig] = spaceConditions
      override def cutOffDistance: Double = cutOff
    }
  }
}