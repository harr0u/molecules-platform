package state.cells

import domain.geometry.figures.CubicFigure

object ParticlesCellMetadata {
  def fromCubicFigure(fig: CubicFigure, minimumCellLength: Double = 5.0): ParticlesCellMetadata = {
    val findNumberOfCellsInDirection = (directionLength: Double) => {
      Math.max(Math.floor(directionLength / minimumCellLength), 1).toInt
    }
    val layersNumber: Int = findNumberOfCellsInDirection(fig.height)
    val rowsNumber: Int = findNumberOfCellsInDirection(fig.length)
    val cellsNumber: Int = findNumberOfCellsInDirection(fig.width)

    ParticlesCellMetadata(
      layersNumber,
      rowsNumber,
      cellsNumber,
      fig.height / layersNumber,
      fig.length / rowsNumber,
      fig.width / cellsNumber,
    )
  }
}

case class ParticlesCellMetadata(
  layersNumber: Int,
  rowsNumber: Int,
  cellsNumber: Int,
  cellHeight: Double,
  cellLength: Double,
  cellWidth: Double
)