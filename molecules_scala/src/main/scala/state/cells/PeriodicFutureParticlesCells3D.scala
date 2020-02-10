package state.cells

import java.util.concurrent.ExecutorService

import calculation.limit_conditions.LimitConditions
import domain.Particle
import domain.geometry.figures.CubicFigure
import domain.geometry.vector.Vector3D
import state.ParticlesState
import state.cells.ParticlesCellsMetadata.Tuple3Same
import state.cells.PeriodicFutureParticlesCells3D.{Cell, Cells}

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class PeriodicFutureParticlesCells3D(
  limitConditions: LimitConditions[Vector3D, CubicFigure],
  cellsMetadata: ParticlesCells3DMetadata,
  private val currentFlatCells: Vector[Seq[Particle[Vector3D]]],
  minimumCellLength: Double = 5.0
) extends PeriodicParticleCells[Vector3D, Future, Tuple3Same](cellsMetadata) {

  override def unit(): Future[ParticlesState[Vector3D, Future]] = {
    Future.successful(this)
  }

  override def counit: LazyList[Particle[Vector3D]] = currentFlatCells.reduce(_ ++ _).to(LazyList)

  override def map(mapFn: Particle[Vector3D] => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val mapFutures: Seq[Future[(Seq[Particle[Vector3D]], Seq[(Int, Particle[Vector3D])])]] = for {
      (cell, index) <- currentFlatCells.zipWithIndex
    } yield {
      Future[(Cell, Seq[(Int, Particle[Vector3D])])] {
        super.mapCellWithIndex(cell, index, mapFn)
      }
    }

    for (computedRawFlatCells <- Future.sequence(mapFutures)) yield {
      this.copy(currentFlatCells = sewMappedCells(computedRawFlatCells))
    }
  }

  override def reduce(reduceFn: (Particle[Vector3D], Particle[Vector3D]) => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val reduceFutures: Vector[Future[Seq[Particle[Vector3D]]]] = for ((cell, index) <- currentFlatCells.zipWithIndex) yield {
      Future[Cell] {
        super.reduceCellWithIndex(cell, index, reduceFn)
      }
    }

    Future.sequence(reduceFutures).map((newFlatCells) => this.copy(currentFlatCells = newFlatCells))
  }

  def getAdjacentCells(flatIndex: Int): Seq[Cell] = {
    (for {
      (layerIndex, rowIndex, cellIndex) <- cellsMetadata.flatIndex2Indexes(flatIndex)
    } yield {
      for {
        deltaL <- -1 to 1
        deltaR <- -1 to 1
        deltaC <- -1 to 1
      } yield {
        val getPeriodicAdjIndex = (index: Int, delta: Int, number: Int) => {
          val index_ = (index + delta) % number
          if (index_ < 0) number + index_ else index_
        }
        val (layersNumber, rowsNumber, cellsNumber) = this.cellsMetadata.cellsNumber
        (for {
          flatIndex <- cellsMetadata.indexes2FlatIndex(
            getPeriodicAdjIndex(layerIndex, deltaL, layersNumber),
            getPeriodicAdjIndex(rowIndex, deltaR, rowsNumber),
            getPeriodicAdjIndex(cellIndex, deltaC, cellsNumber)
          )
        } yield {
          this.currentFlatCells.applyOrElse[Int, Seq[Particle[Vector3D]]](flatIndex, (_) => Seq())
        }) getOrElse {
          Seq()
        }
      }
    }) getOrElse {
      Seq()
    }
  }

  override def getFlatCellIndexOfParticle(particle: Particle[Vector3D]): Option[Int] = {
    cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y, particle.position.z))
  }
}

object PeriodicFutureParticlesCells3D {
  type Cell = Seq[Particle[Vector3D]] // Maybe mutable?
  type Cells = Vector[Cell];

  def create(particles: Seq[Particle[Vector3D]], limitConditions: LimitConditions[Vector3D, CubicFigure], minimumCellLength: Double = 5.0): PeriodicFutureParticlesCells3D = {
    val cellsMetadata = ParticlesCellMetadata.fromCubicFigure(limitConditions.boundaries)

    PeriodicFutureParticlesCells3D(limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells3DMetadata): Cells = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    (for {
      _ <- (1 to layersNumber)
      _ <- (1 to rowsNumber)
      _ <- (1 to cellsNumber)
    } yield Seq[Particle[Vector3D]]()).toVector;
  }

  def makeFlatCells(cellsMetadata: ParticlesCells3DMetadata, particles: Seq[Particle[Vector3D]]): Cells = {
    particles.foldLeft(makeEmptyFlatCells(cellsMetadata))(
      (accCells: Vector[Seq[Particle[Vector3D]]], particle: Particle[Vector3D]) => {
        (for {
          i <- cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y, particle.position.z))
        } yield (
          accCells.updated(i, accCells(i).appended(particle))
          )).getOrElse(accCells)
      }
    )
  }
}

