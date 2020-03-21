package state.state.cells

import calculation.space.SpaceConditions
import cats.{Monad, Parallel, Traverse}
import domain.Particle
import domain.geometry.figures.RectangleFigure
import domain.geometry.vector.Vector2D
import simulation.ParticlesState
import state.state.cells.ParticlesCellsMetadata.Tuple2Same
import state.state.cells.PeriodicParticlesCells.ListList

import scala.collection.immutable.Seq

case class PeriodicParticlesCells2D[Context[_] : Monad](
                                                    limitConditions: SpaceConditions[Vector2D, RectangleFigure],
                                                    cellsMetadata: ParticlesCells2DMetadata,
                                                    override val counit: List[List[Particle[Vector2D]]],
                                                    minimumCellLength: Double = 5.0
                                                  )(implicit par : Parallel[Context]) extends PeriodicParticlesCells[Vector2D, Context, Tuple2Same] {

  override def getAdjacentCells(flatIndex: Int): List[List[Particle[Vector2D]]] = {
    val (rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    cellsMetadata.flatIndex2Indexes(flatIndex)
      .map {
        case (rowIndex, cellIndex) =>
          (for (deltaR <- -1 to 1; deltaC <- -1 to 1) yield {
            cellsMetadata.indexes2FlatIndex(
              getPeriodicAdjIndex(rowIndex + deltaR, rowsNumber),
              getPeriodicAdjIndex(cellIndex + deltaC, cellsNumber)
            )
              .map(flatIndex => this.counit.applyOrElse[Int, List[Particle[Vector2D]]](flatIndex, (_) => List()))
              .getOrElse(List())
          }).toList
      }
      .getOrElse(List())
  }

  override def getFlatCellIndexOfParticle(particle: Particle[Vector2D]): Option[Int] = {
    cellsMetadata.getFlatCellIndexByCoordinate(particle.position.x, particle.position.y)
  }

  override def updateWithParticles(currentFlatCells: List[List[Particle[Vector2D]]]): Context[ParticlesState[Vector2D, Context, ListList]] = {
    Context.pure(this.copy(counit = currentFlatCells))
  }
}

object PeriodicParticlesCells2D {
  def create[Context[_] : Monad](particles: Seq[Particle[Vector2D]], limitConditions: SpaceConditions[Vector2D, RectangleFigure], minimumCellLength: Double = 5.0)
                          (implicit P: Parallel[Context]): PeriodicParticlesCells2D[Context] = {
    val cellsMetadata = ParticlesCellMetadata.fromRectangleFigure(limitConditions.boundaries)

    PeriodicParticlesCells2D[Context](limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells2DMetadata): List[List[Particle[Vector2D]]] = {
    val (rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    for {
      _ <- (1 to rowsNumber).toList
      _ <- (1 to cellsNumber).toList
    } yield List[Particle[Vector2D]]()
  }

  def makeFlatCells(cellsMetadata: ParticlesCells2DMetadata, particles: Seq[Particle[Vector2D]]): List[List[Particle[Vector2D]]] = {
    particles
      .foldLeft(makeEmptyFlatCells(cellsMetadata))(
        (accCells: List[List[Particle[Vector2D]]], particle: Particle[Vector2D]) => {
          cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y))
            .map(i => accCells.updated(i, accCells(i) :+ particle))
            .getOrElse(accCells)
        }
      )
  }
}

