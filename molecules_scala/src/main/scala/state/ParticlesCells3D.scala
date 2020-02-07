package state

import domain.geometry.figures.CubicFigure
import calculation.limit_conditions.LimitConditions
import domain.Particle
import domain.geometry.vector.Vector3D

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

case class ParticlesCells3D(
  limitConditions: LimitConditions[Vector3D, CubicFigure],
  cellsMetadata: ParticlesCellMetadata,
  private val currentFlatCells: Seq[Seq[Particle[Vector3D]]],
  minimumCellLength: Double = 5.0
) extends ParticlesState[Vector3D, Future] {

  override def unit(particles: Seq[Particle[Vector3D]]): Future[ParticlesState[Vector3D, Future]] = {
    Future.successful(ParticlesCells3D.create(particles, limitConditions))
  }
  override def counit: LazyList[Particle[Vector3D]] = currentFlatCells.reduce(_ ++ _).to(LazyList)

  override def map(fn: Particle[Vector3D] => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val mapFutures: Seq[Future[Seq[Particle[Vector3D]]]] = for (cell: ParticlesCells3D.Cell <- currentFlatCells) yield {
      Future[Seq[Particle[Vector3D]]] {
        for (particle: Particle[Vector3D] <- cell) yield fn(particle);
      }
    }

    Future.sequence(mapFutures).map((newFlatCells) => this.copy(currentFlatCells = newFlatCells))
  }

  override def particlesReduce(fn: (Particle[Vector3D], Particle[Vector3D]) => Particle[Vector3D]): Future[ParticlesState[Vector3D, Future]] = {
    val reduceFutures: Seq[Future[Seq[Particle[Vector3D]]]] = for (cell: ParticlesCells3D.Cell <- currentFlatCells) yield {
      Future[Seq[Particle[Vector3D]]] {
        // getAdjacent cells
        for (particle: Particle[Vector3D] <- cell) yield fn(particle);
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

object ParticlesCells3D {
  type Cell = Seq[Particle[Vector3D]] // Maybe mutable?
  type Cells = Vector[Cell];

  def create(particles: Seq[Particle[Vector3D]], limitConditions: LimitConditions[Vector3D, CubicFigure], minimumCellLength: Double = 5.0): ParticlesCells3D = {
    val cellsMetadata = ParticlesCellMetadata.fromCubicFigure(limitConditions.boundaries)

    ParticlesCells3D(particles, limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
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
          i <- getFlatCellIndexOfParticle(cellsMetadata, particle)
        } yield {
          accCells.updated(i, accCells(i).appended(particle)
        }).getOrElse(accCells)
      }
    )
  }

  protected def getFlatCellIndexOfParticle(cellsMetadata: ParticlesCellMetadata, particle: Particle[Vector3D]): Option[Int] = {
    val i = Math.floor(particle.position.x / cellsMetadata.cellWidth).toInt
    val j = Math.floor(particle.position.y / cellsMetadata.cellLength).toInt
    val k = Math.floor(particle.position.z / cellsMetadata.cellHeight).toInt

    if ((i >= 0 && i < cellsMetadata.cellsNumber)
      && (j >= 0 && j < cellsMetadata.rowsNumber)
      && (k >= 0 && k < cellsMetadata.layersNumber)
    ) {
      Some((k * cellsMetadata.cellsNumber * cellsMetadata.rowsNumber) + (j * cellsMetadata.cellsNumber) + i)
    } else {
      None
    }
  }
}

