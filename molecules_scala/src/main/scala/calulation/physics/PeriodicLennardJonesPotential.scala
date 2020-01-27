package calculation.physics

import domain.Particle
import domain.vector._

// I need some container box parametera
// TODO: make class for box parameters
class PeriodicLennardJonesPotential[VectorType <: AlgebraicVector[VectorType]](
  val boxWidth: Double
) extends PotentialCalculator[VectorType] {
  def computeForceAndPotential(positionX: VectorType, positionY: VectorType): (VectorType, Double) = {
    val periodicConditionForCoordinate = (coordinate: Double) => if (Math.abs(coordinate) > boxWidth / 2) {
      -1 * Math.signum(coordinate) * (boxWidth - Math.abs(coordinate))
    } else {
      coordinate
    }
    val radiusVector: VectorType = (positionY - positionX).mapCoordinates(periodicConditionForCoordinate)

    val radiusVectorInverted: Double = 1.0 / radiusVector.length
    val r6: Double = Math.pow(radiusVectorInverted, 6)
    val r12: Double = r6 * r6

    val force: VectorType = radiusVector * (-48 * (r12 * radiusVectorInverted * radiusVectorInverted - 0.5 * r6 * radiusVectorInverted * radiusVectorInverted))
    val potential: Double = 2 * (r12 - r6)
    
    (force, potential)
  }
}