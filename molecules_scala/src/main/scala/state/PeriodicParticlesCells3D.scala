package state

import domain.geometry.figures.CubicFigure
import calculation.limit_conditions.LimitConditions
import domain.Particle
import domain.geometry.vector.Vector3D

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

case class PeriodicParticlesCells3D(
  limitConditions: LimitConditions[Vector3D, CubicFigure],
  cellsMetadata: ParticlesCellMetadata,
  private val currentFlatCells: Vector[Seq[Particle[Vector3D]]],
  minimumCellLength: Double = 5.0
) extends ParticlesState[Vector3D, Future] {

  override def unit(): Future[ParticlesState[Vector3D, Future]] = {
    Future.successful(this)
  }
  override def counit: LazyList[Particle[Vector3D]] = currentFlatCells.reduce(_ ++ _).to(LazyList)

  override def map(fn: Particle[Vector3D] => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val mapFutures: Seq[Future[(Queue[Particle[Vector3D]], Queue[(Particle[Vector3D], Int)])]] = for {
      (cell, i) <- currentFlatCells.zipWithIndex
    } yield {
      Future[(Queue[Particle[Vector3D]], Queue[(Particle[Vector3D], Int)])] {
        cell.foldLeft((Queue[Particle[Vector3D]](), Queue[(Particle[Vector3D], Int)]()))((acc, particle) => {
          val (rest, left) = acc

          val newParticle = fn(particle)
          if (newParticle.position == particle.position) {
            (rest.appended(newParticle), left)
          } else {
            PeriodicParticlesCells3D.getFlatCellIndexOfParticle(this.cellsMetadata)(newParticle)
              .map((newIndex) => (rest, left.appended((newParticle, newIndex))))
              .getOrElse((rest.appended(newParticle), left))
          }
        })
      }
    }

    Future.sequence(mapFutures).map(computedFlatCells => {
      val restParticlesFromCells: Vector[Seq[Particle[Vector3D]]] = computedFlatCells.map(_._1).toVector
      val leftParticlesFromCells: Seq[Queue[(Particle[Vector3D], Int)]] = computedFlatCells.map(_._2)

      val newParticleCells: Vector[Seq[Particle[Vector3D]]] = leftParticlesFromCells.foldLeft(restParticlesFromCells)((particlesAcc, leftParticlesFromCell) => {
        leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, leftParticleWithIndex) => {
          val (leftParticle, newCellIndex) = leftParticleWithIndex
          particlesAcc1.updated(newCellIndex, particlesAcc1(newCellIndex).appended(leftParticle))
        })
      })

      this.copy(currentFlatCells = newParticleCells)
    })
  }

  // TODO implement reduce via map to use update particle cell logic
  override def particlesReduce(fn: (Particle[Vector3D], Particle[Vector3D]) => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val reduceFutures: Vector[Future[Seq[Particle[Vector3D]]]] = for ((cell, i) <- currentFlatCells.zipWithIndex) yield {
      Future[Seq[Particle[Vector3D]]] {
        PeriodicParticlesCells3D.flatIndex2Indexes(this.cellsMetadata)(i) map { case (layerIndex, rowIndex, cellIndex) => {
          // TODO should use limit conditions to determine what to do in corner cases (-1, -1, 0)?
          val cellsInvolvedInComputation: Seq[Seq[Particle[Vector3D]]] = for {
            deltaL <- -1 to 1
            deltaR <- -1 to 1
            deltaC <- -1 to 1
          } yield {
            val getPeriodicAdjIndex = (index: Int, delta: Int, number: Int) => {
              val index_ = (index + delta) % number
              if (index_ < 0) number + index_ else index_
            }

            (for {
              flatIndex <- PeriodicParticlesCells3D.indexes2FlatIndex(this.cellsMetadata)(
                getPeriodicAdjIndex(layerIndex, deltaL, this.cellsMetadata.layersNumber),
                getPeriodicAdjIndex(rowIndex, deltaR, this.cellsMetadata.rowsNumber),
                getPeriodicAdjIndex(cellIndex, deltaC, this.cellsMetadata.cellsNumber)
              )
            } yield {
              this.currentFlatCells.applyOrElse[Int, Seq[Particle[Vector3D]]](flatIndex, (_) => List())
            }) getOrElse {
              List()
            }
          }

          cell.map((particle) => {
            cellsInvolvedInComputation.foldLeft(particle)((accParticle, cell) => {
              cell.foldLeft(accParticle)((accParticle1, otherParticle) => {
                if (accParticle1.id != otherParticle.id) fn(accParticle1, otherParticle) else accParticle1
              })
            })
          })
        }} getOrElse {
          cell
        }
      }
    }

    Future.sequence(reduceFutures).map((newFlatCells) => this.copy(currentFlatCells = newFlatCells))
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

object PeriodicParticlesCells3D {
  type Cell = Seq[Particle[Vector3D]] // Maybe mutable?
  type Cells = Vector[Cell];

  def create(particles: Seq[Particle[Vector3D]], limitConditions: LimitConditions[Vector3D, CubicFigure], minimumCellLength: Double = 5.0): PeriodicParticlesCells3D = {
    val cellsMetadata = ParticlesCellMetadata.fromCubicFigure(limitConditions.boundaries)

    PeriodicParticlesCells3D(limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCellMetadata): Cells = {
    (for {
      _ <- (1 to cellsMetadata.layersNumber)
      _ <- (0 to cellsMetadata.rowsNumber)
      _ <- (0 to cellsMetadata.cellsNumber)
    } yield Seq[Particle[Vector3D]]()).toVector;
  }

  def makeFlatCells(cellsMetadata: ParticlesCellMetadata, particles: Seq[Particle[Vector3D]]): Cells = {
    particles.foldLeft(makeEmptyFlatCells(cellsMetadata))(
      (accCells: Vector[Seq[Particle[Vector3D]]], particle: Particle[Vector3D]) => {
        (for {
          i <- getFlatCellIndexOfParticle(cellsMetadata)(particle)
        } yield (
          accCells.updated(i, accCells(i).appended(particle))
        )).getOrElse(accCells)
      }
    )
  }

  protected def getFlatCellIndexOfParticle(cellsMetadata: ParticlesCellMetadata)(particle: Particle[Vector3D]): Option[Int] = {
    indexes2FlatIndex(cellsMetadata)(
      Math.floor(particle.position.z / cellsMetadata.cellHeight).toInt,
      Math.floor(particle.position.y / cellsMetadata.cellLength).toInt,
      Math.floor(particle.position.x / cellsMetadata.cellWidth).toInt
    )
  }

  protected def flatIndex2Indexes(cellsMetadata: ParticlesCellMetadata)(index: Int): Option[(Int, Int, Int)] = {
    val numberOfCellsInLayer = cellsMetadata.cellsNumber * cellsMetadata.rowsNumber

    val layerIndex: Int = Math.floor(index / numberOfCellsInLayer).toInt;
    if (layerIndex >= 0 && layerIndex < cellsMetadata.layersNumber) {
      val lastLayerIndex = index - numberOfCellsInLayer * layerIndex

      val rowIndex: Int = Math.floor(lastLayerIndex / cellsMetadata.cellsNumber).toInt
      if (rowIndex >= 0 && rowIndex < cellsMetadata.rowsNumber) {

        val cellIndex = lastLayerIndex - cellsMetadata.cellsNumber * rowIndex
        if (cellIndex >= 0 && cellIndex < cellsMetadata.cellsNumber) {
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

  protected def indexes2FlatIndex(cellsMetadata: ParticlesCellMetadata)(layerIndex: Int, rowIndex: Int, cellIndex: Int): Option[Int] = {
    if ((cellIndex >= 0 && cellIndex < cellsMetadata.cellsNumber)
      && (rowIndex >= 0 && rowIndex < cellsMetadata.rowsNumber)
      && (layerIndex >= 0 && layerIndex < cellsMetadata.layersNumber)
    ) {
      Some((layerIndex * cellsMetadata.cellsNumber * cellsMetadata.rowsNumber) + (rowIndex * cellsMetadata.cellsNumber) + cellIndex)
    } else {
      None
    }
  }
}

