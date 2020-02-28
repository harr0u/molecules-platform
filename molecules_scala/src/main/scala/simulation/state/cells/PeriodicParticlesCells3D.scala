package state.state.cells

import calculation.space.SpaceConditions
import cats.implicits._
import cats.{Monad, Parallel, Traverse}
import domain.Particle
import domain.geometry.figures.CubicFigure
import domain.geometry.vector.Vector3D
import state.state.cells.ParticlesCellsMetadata.Tuple3Same
import state.state.cells.PeriodicParticleCells.{Cell, Cells}

import scala.collection.immutable.Seq

case class PeriodicParticlesCells3D[F[_] : Monad](
                                           limitConditions: SpaceConditions[Vector3D, CubicFigure],
                                           cellsMetadata: ParticlesCells3DMetadata,
                                           override val currentFlatCells: Cells[Vector3D],
                                           minimumCellLength: Double = 5.0
)(implicit par : Parallel[F]) extends PeriodicParticleCells[Vector3D, F, Tuple3Same] {

  override def getAdjacentCells(flatIndex: Int): Seq[Cell[Vector3D]] = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    cellsMetadata.flatIndex2Indexes(flatIndex).map {
      case (layerIndex, rowIndex, cellIndex) =>
        for (deltaR <- -1 to 1; deltaC <- -1 to 1; deltaL <- -1 to 1) yield {
          cellsMetadata.indexes2FlatIndex(
            getPeriodicAdjIndex(layerIndex + deltaL, layersNumber),
            getPeriodicAdjIndex(rowIndex + deltaR, rowsNumber),
            getPeriodicAdjIndex(cellIndex + deltaC, cellsNumber)
          )
            .map(flatIndex => this.currentFlatCells.applyOrElse[Int, Cell[Vector3D]](flatIndex, (_) => Seq()))
            .getOrElse(Seq())
        }
    }
      .getOrElse(Seq())
  }

  override def getFlatCellIndexOfParticle(particle: Particle[Vector3D]): Option[Int] = {
    cellsMetadata.getFlatCellIndexByCoordinate(particle.position.x, particle.position.y, particle.position.z)
  }

  override def updateWithFlatCells(currentFlatCells: Cells[Vector3D]): PeriodicParticleCells[Vector3D, F, Tuple3Same] = {
    this.copy[F](currentFlatCells = currentFlatCells)
  }
}

object PeriodicParticlesCells3D {
  def create[F[_] : Monad](particles: Seq[Particle[Vector3D]], limitConditions: SpaceConditions[Vector3D, CubicFigure], minimumCellLength: Double = 5.0)
                          (implicit par : Parallel[F]): PeriodicParticlesCells3D[F] = {
    val cellsMetadata = ParticlesCellMetadata.fromCubicFigure(limitConditions.boundaries)

    PeriodicParticlesCells3D[F](limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells3DMetadata): Cells[Vector3D] = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    for {
      _ <- 1 to layersNumber
      _ <- 1 to rowsNumber
      _ <- 1 to cellsNumber
    } yield Cell[Vector3D]()
  }

  def makeFlatCells(cellsMetadata: ParticlesCells3DMetadata, particles: Seq[Particle[Vector3D]]): Cells[Vector3D] = {
    particles.foldLeft(makeEmptyFlatCells(cellsMetadata))(
      (accCells: Cells[Vector3D], particle: Particle[Vector3D]) => {
        (for {
          i <- cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y, particle.position.z))
        } yield (
          accCells.updated(i, accCells(i) :+ particle)
          )).getOrElse(accCells)
      }
    )
  }
}
