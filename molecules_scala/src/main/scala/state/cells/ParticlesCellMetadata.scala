package state.cells

import domain.geometry.figures.{CubicFigure, RectangleFigure}
import state.cells.ParticlesCellsMetadata.{Tuple2Same, Tuple3Same}

object ParticlesCellMetadata {
  private def findNumberOfCellsInDirection(directionLength: Double, minimumCellLength: Double = 5.0): Int = {
    Math.max(Math.floor(directionLength / minimumCellLength), 1).toInt
  }

  def fromCubicFigure(fig: CubicFigure, minimumCellLength: Double = 5.0): ParticlesCells3DMetadata = {
    val layersNumber: Int = findNumberOfCellsInDirection(fig.height)
    val rowsNumber: Int = findNumberOfCellsInDirection(fig.length)
    val cellsNumber: Int = findNumberOfCellsInDirection(fig.width)

    ParticlesCells3DMetadata(
      (layersNumber, rowsNumber, cellsNumber),
      (fig.height / layersNumber, fig.length / rowsNumber, fig.width / cellsNumber)
    )
  }

  def fromRectangleFigure(fig: RectangleFigure, minimumCellLength: Double = 5.0): ParticlesCells2DMetadata = {
    val rowsNumber: Int = findNumberOfCellsInDirection(fig.length)
    val cellsNumber: Int = findNumberOfCellsInDirection(fig.width)

    ParticlesCells2DMetadata(
      (rowsNumber, cellsNumber),
      (fig.length / rowsNumber, fig.width / cellsNumber)
    )
  }
}

case class ParticlesCells2DMetadata(
  cellsNumber: (Int, Int),
  cellsSize: (Double, Double)
) extends ParticlesCellsMetadata[Tuple2Same] {
  override def flatIndex2Indexes(flatIndex: Int): Option[(Int, Int)] = {
    val (rowsNumber, cellsNumber) = this.cellsNumber

    val rowIndex: Int = Math.floor(flatIndex / cellsNumber).toInt
    if (rowIndex >= 0 && rowIndex < rowsNumber) {

      val cellIndex = rowIndex - cellsNumber * rowIndex
      if (cellIndex >= 0 && cellIndex < cellsNumber) {
        Some((rowIndex, cellIndex))
      } else {
        None
      }
    } else {
      None
    }
  }

  def indexes2FlatIndex(indexes: (Int, Int)): Option[Int] = {
    val (rowIndex: Int, cellIndex: Int) = indexes
    val (rowsNumber: Int, cellsNumber: Int) = this.cellsNumber

    if ((cellIndex >= 0 && cellIndex < cellsNumber)
      && (rowIndex >= 0 && rowIndex < rowsNumber)
    ) {
      Some((rowIndex * cellsNumber) + cellIndex)
    } else {
      None
    }
  }

  override def getFlatCellIndexByCoordinate(coordinates: (Double, Double)): Option[Int] = {
    indexes2FlatIndex((
      Math.floor(coordinates._2 / cellsSize._1).toInt,
      Math.floor(coordinates._1 / cellsSize._2).toInt
    ))
  }

}

case class ParticlesCells3DMetadata(
  cellsNumber: (Int, Int, Int),
  cellsSize: (Double, Double, Double)
) extends ParticlesCellsMetadata[Tuple3Same] {

  override def flatIndex2Indexes(flatIndex: Int): Option[(Int, Int, Int)] = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = this.cellsNumber

    val numberOfCellsInLayer = cellsNumber * rowsNumber

    val layerIndex: Int = Math.floor(flatIndex / numberOfCellsInLayer).toInt;
    if (layerIndex >= 0 && layerIndex < layersNumber) {
      val lastLayerIndex = flatIndex - numberOfCellsInLayer * layerIndex

      val rowIndex: Int = Math.floor(lastLayerIndex / cellsNumber).toInt
      if (rowIndex >= 0 && rowIndex < rowsNumber) {

        val cellIndex = lastLayerIndex - cellsNumber * rowIndex
        if (cellIndex >= 0 && cellIndex < cellsNumber) {
          Some((layerIndex, rowIndex, cellIndex))
        } else {
          None
        }
      } else {
        None
      }
    } else {
      None
    }
  }
  def indexes2FlatIndex(indexes: (Int, Int, Int)): Option[Int] = {
    val (layerIndex: Int, rowIndex: Int, cellIndex: Int) = indexes
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = this.cellsNumber

    if ((cellIndex >= 0 && cellIndex < cellsNumber)
      && (rowIndex >= 0 && rowIndex < rowsNumber)
      && (layerIndex >= 0 && layerIndex < layersNumber)
    ) {
      Some((layerIndex * cellsNumber * rowsNumber) + (rowIndex * cellsNumber) + cellIndex)
    } else {
      None
    }
  }

  override def getFlatCellIndexByCoordinate(coordinates: (Double, Double, Double)): Option[Int] = {
    indexes2FlatIndex((
      Math.floor(coordinates._3 / cellsSize._1).toInt,
      Math.floor(coordinates._2 / cellsSize._2).toInt,
      Math.floor(coordinates._1 / cellsSize._3).toInt
    ))
  }
}



trait ParticlesCellsMetadata[T[_]] {
  def cellsNumber: T[Int]
  def cellsSize: T[Double]

  def getFlatCellIndexByCoordinate(coordinates: T[Double]): Option[Int]
  def flatIndex2Indexes(flatIndex: Int): Option[T[Int]]
  def indexes2FlatIndex(indexes: T[Int]): Option[Int]
}

object  ParticlesCellsMetadata {
  type Tuple3Same[T] = (T, T, T)
  type Tuple2Same[T] = (T, T)
}
