package state.state.cells

import calculation.space.SpaceConditions
import cats.{Monad, Parallel, Traverse}
import domain.Particle
import domain.geometry.figures.RectangleFigure
import domain.geometry.vector.Vector2D
import simulation.ParticlesState
import state.state.cells.ParticlesCellsMetadata.Tuple2Same
import state.state.cells.PeriodicParticleCells.{Cell, Cells, SeqSeq}

import scala.collection.immutable.Seq

case class PeriodicParticlesCells2D[F[_] : Monad](
                                                    limitConditions: SpaceConditions[Vector2D, RectangleFigure],
                                                    cellsMetadata: ParticlesCells2DMetadata,
                                                    override val currentFlatCells: Cells[Vector2D],
                                                    minimumCellLength: Double = 5.0
                                                  )(implicit par : Parallel[F]) extends PeriodicParticleCells[Vector2D, F, Tuple2Same] {

  override def getAdjacentCells(flatIndex: Int): Seq[Cell[Vector2D]] = {
    val (rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    cellsMetadata.flatIndex2Indexes(flatIndex)
      .map {
        case (rowIndex, cellIndex) =>
          for (deltaR <- -1 to 1; deltaC <- -1 to 1) yield {
            cellsMetadata.indexes2FlatIndex(
              getPeriodicAdjIndex(rowIndex + deltaR, rowsNumber),
              getPeriodicAdjIndex(cellIndex + deltaC, cellsNumber)
            )
              .map(flatIndex => this.currentFlatCells.applyOrElse[Int, Cell[Vector2D]](flatIndex, (_) => Seq()))
              .getOrElse(Seq())
          }
      }
      .getOrElse(Seq())
  }

  override def getFlatCellIndexOfParticle(particle: Particle[Vector2D]): Option[Int] = {
    cellsMetadata.getFlatCellIndexByCoordinate(particle.position.x, particle.position.y)
  }

  override def updateWithFlatCells(currentFlatCells: Cells[Vector2D]): PeriodicParticleCells[Vector2D, F, Tuple2Same] = {
    this.copy(currentFlatCells = currentFlatCells)
  }
}

object PeriodicParticlesCells2D {
  def create[F[_] : Monad](particles: Seq[Particle[Vector2D]], limitConditions: SpaceConditions[Vector2D, RectangleFigure], minimumCellLength: Double = 5.0)
                          (implicit par : Parallel[F]): PeriodicParticlesCells2D[F] = {
    val cellsMetadata = ParticlesCellMetadata.fromRectangleFigure(limitConditions.boundaries)

    PeriodicParticlesCells2D[F](limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells2DMetadata): Cells[Vector2D] = {
    val (rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    for {
      _ <- (1 to rowsNumber)
      _ <- (1 to cellsNumber)
    } yield Cell[Vector2D]()
  }

  def makeFlatCells(cellsMetadata: ParticlesCells2DMetadata, particles: Seq[Particle[Vector2D]]): Cells[Vector2D] = {
    particles
      .foldLeft(makeEmptyFlatCells(cellsMetadata))(
        (accCells: Cells[Vector2D], particle: Particle[Vector2D]) => {
          cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y))
            .map(i => accCells.updated(i, accCells(i) :+ particle))
            .getOrElse(accCells)
        }
      )
  }
}



