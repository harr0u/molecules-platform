package state.cells

import calculation.limit_conditions.LimitConditions
import domain.Particle
import domain.geometry.figures.CubicFigure
import domain.geometry.vector.Vector3D
import state.ParticlesState

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
    val mapFutures: Seq[Future[(Seq[Particle[Vector3D]], Seq[(Int, Particle[Vector3D])])]] = for {
      (cell, i) <- currentFlatCells.zipWithIndex
    } yield {
      Future[(Seq[Particle[Vector3D]], Seq[(Int, Particle[Vector3D])])] {
        cell.foldLeft((Seq[Particle[Vector3D]](), Seq[(Int, Particle[Vector3D])]()))((acc, particle) => {
          val (rest, left) = acc

          val newParticle = fn(particle)
          if (newParticle.position == particle.position) {
            (rest.appended(newParticle), left)
          } else {
            (for {
              newIndex <- PeriodicParticlesCells3D.getFlatCellIndexOfParticle(this.cellsMetadata)(newParticle)
              if (newIndex) != i
            } yield {
              (rest, left.appended((newIndex, newParticle)))
            }) getOrElse {
              (rest.appended(newParticle), left)
            }
          }
        })
      }
    }

    Future.sequence(mapFutures).map(computedFlatCells => {
      val restParticlesFromCells: Vector[Seq[Particle[Vector3D]]] = computedFlatCells.map(_._1).toVector
      val leftParticlesFromCells: Seq[Seq[(Int, Particle[Vector3D])]] = computedFlatCells.map(_._2)

      val newParticleCells: Vector[Seq[Particle[Vector3D]]] = leftParticlesFromCells.foldLeft(restParticlesFromCells)((particlesAcc, leftParticlesFromCell) => {
        leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, leftParticleWithIndex) => {
          val (newCellIndex, leftParticle) = leftParticleWithIndex
          particlesAcc1.updated(newCellIndex, particlesAcc1(newCellIndex).appended(leftParticle))
        })
      })

      this.copy(currentFlatCells = newParticleCells)
    })
  }

  // TODO implement reduce via map to use update particle cell logic
  override def reduce(fn: (Particle[Vector3D], Particle[Vector3D]) => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
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
              this.currentFlatCells.applyOrElse[Int, Seq[Particle[Vector3D]]](flatIndex, (_) => Seq())
            }) getOrElse {
              Seq()
            }
          }

          cell.map((particle) => {
            cellsInvolvedInComputation.foldLeft(particle)((accParticle, cell) => {
              cell.foldLeft(accParticle)((accParticle1, otherParticle) => {
                if (accParticle1.id != otherParticle.id) fn(accParticle1, otherParticle) else accParticle1
              })
            })
          })
        }
        } getOrElse {
          cell
        }
      }
    }

    Future.sequence(reduceFutures).map((newFlatCells) => this.copy(currentFlatCells = newFlatCells))
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

