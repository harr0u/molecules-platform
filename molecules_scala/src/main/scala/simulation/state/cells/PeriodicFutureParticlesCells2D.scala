package state.state.cells

import calculation.limitConditions.SpaceConditions
import domain.Particle
import domain.geometry.figures.RectangleFigure
import domain.geometry.vector.Vector2D
import simulation.ParticlesState
import state.state.cells.ParticlesCellsMetadata.Tuple2Same
import state.state.cells.PeriodicParticleCells.{Cells, Cell}

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}

case class PeriodicFutureParticlesCells2D(
                                           limitConditions: SpaceConditions[Vector2D, RectangleFigure],
                                           cellsMetadata: ParticlesCells2DMetadata,
                                           private val currentFlatCells: Cells[Vector2D],
                                           minimumCellLength: Double = 5.0
) extends PeriodicParticleCells[Vector2D, Future, Tuple2Same](cellsMetadata) {

  override def getParticles: List[Particle[Vector2D]] = currentFlatCells.reduce(_ ++ _).to(List)

  override def map(mapFn: Particle[Vector2D] => Particle[Vector2D]): Future[ParticlesState[Vector2D, Future]] = {
    val mapFutures: Seq[Future[(Seq[Particle[Vector2D]], Seq[(Int, Particle[Vector2D])])]] = for {
      (cell, index) <- currentFlatCells.zipWithIndex
    } yield {
      Future[(Cell[Vector2D], Seq[(Int, Particle[Vector2D])])] {
        super.mapCellWithIndex(cell, index, mapFn)
      }
    }

    for (computedRawFlatCells <- Future.sequence(mapFutures)) yield {
      this.copy(currentFlatCells = sewMappedCellsIntoFlatCells(computedRawFlatCells))
    }
  }

  override def reduce(reduceFn: (Particle[Vector2D], Particle[Vector2D]) => Particle[Vector2D]): Future[ParticlesState[Vector2D, Future]] = {
    val reduceFutures: Seq[Future[Cell[Vector2D]]] = for ((cell, index) <- currentFlatCells.zipWithIndex) yield {
      Future[Cell[Vector2D]] {
        super.reduceCellWithIndex(cell, index, reduceFn)
    }}

    Future.sequence(reduceFutures).map((newFlatCells) => this.copy(currentFlatCells = newFlatCells))
  }

  override def getAdjacentCells(flatIndex: Int): Seq[Cell[Vector2D]] = {
    val (rowsNumber, cellsNumber) = this.cellsMetadata.cellsNumber

    cellsMetadata.flatIndex2Indexes(flatIndex).map {
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
    cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y))
  }
}

object PeriodicFutureParticlesCells2D {
  def create(particles: Seq[Particle[Vector2D]], limitConditions: SpaceConditions[Vector2D, RectangleFigure], minimumCellLength: Double = 5.0): PeriodicFutureParticlesCells2D = {
    val cellsMetadata = ParticlesCellMetadata.fromRectangleFigure(limitConditions.boundaries)

    PeriodicFutureParticlesCells2D(limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells2DMetadata): Cells[Vector2D] = {
    val (rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    (for {
      _ <- (1 to rowsNumber)
      _ <- (1 to cellsNumber)
    } yield Cell[Vector2D]()).toVector;
  }

  def makeFlatCells(cellsMetadata: ParticlesCells2DMetadata, particles: Seq[Particle[Vector2D]]): Cells[Vector2D] = {
    particles.foldLeft(makeEmptyFlatCells(cellsMetadata))(
      (accCells: Cells[Vector2D], particle: Particle[Vector2D]) => {
        (for {
          i <- cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y))
        } yield (
          accCells.updated(i, accCells(i).appended(particle))
          )).getOrElse(accCells)
      }
    )
  }
}


