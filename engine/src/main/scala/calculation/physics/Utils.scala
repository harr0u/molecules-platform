package calculation.physics

object Utils {
  def calculateWidthWithSideCount(particlesSideNumber: Int, density: Double): Option[Double] = {
    if (density > 0.0 && particlesSideNumber > 0) {
      Some(particlesSideNumber / density)
    } else {
      None
    }
  }

}
