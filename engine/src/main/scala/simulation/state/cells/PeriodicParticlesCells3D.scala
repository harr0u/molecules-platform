package state.state.cells

import calculation.space.SpaceConditions
import cats.implicits._
import cats.{Monad, Parallel, Traverse}
import domain.Particle
import domain.geometry.figures.CubicFigure
import domain.geometry.vector.Vector3D
import simulation.ParticlesState
import state.state.cells.ParticlesCellsMetadata.Tuple3Same
import state.state.cells.PeriodicParticlesCells.ListList

import scala.collection.immutable.Seq

case class PeriodicParticlesCells3D[Context[_] : Monad](
                                           limitConditions: SpaceConditions[Vector3D, CubicFigure],
                                           cellsMetadata: ParticlesCells3DMetadata,
                                           override val counit: ListList[Particle[Vector3D]],
                                           minimumCellLength: Double = 5.0
)(implicit par : Parallel[Context]) extends PeriodicParticlesCells[Vector3D, Context, Tuple3Same] {
  override def getAdjacentCells(flatIndex: Int): List[List[Particle[Vector3D]]] = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    cellsMetadata.flatIndex2Indexes(flatIndex).map {
      case (layerIndex, rowIndex, cellIndex) =>
        (for (deltaR <- -1 to 1; deltaC <- -1 to 1; deltaL <- -1 to 1) yield {
          cellsMetadata.indexes2FlatIndex(
            getPeriodicAdjIndex(layerIndex + deltaL, layersNumber),
            getPeriodicAdjIndex(rowIndex + deltaR, rowsNumber),
            getPeriodicAdjIndex(cellIndex + deltaC, cellsNumber)
          )
            .map(flatIndex => this.counit.applyOrElse[Int, List[Particle[Vector3D]]](flatIndex, (_) => List()))
            .getOrElse(List())
        }).toList
    }
      .getOrElse(List())
  }

  override def getFlatCellIndexOfParticle(particle: Particle[Vector3D]): Option[Int] = {
    cellsMetadata.getFlatCellIndexByCoordinate(particle.position.x, particle.position.y, particle.position.z)
  }

  override def updateWithParticles(flatCells: ListList[Particle[Vector3D]]): Context[ParticlesState[Vector3D, Context, ListList]] = {
    Context.pure(this.copy[Context](counit = flatCells))
  }

}

object PeriodicParticlesCells3D {
  def create[Context[_] : Monad](particles: Seq[Particle[Vector3D]], limitConditions: SpaceConditions[Vector3D, CubicFigure], minimumCellLength: Double = 5.0)
                          (implicit par : Parallel[Context]): PeriodicParticlesCells3D[Context] = {
    val cellsMetadata = ParticlesCellMetadata.fromCubicFigure(limitConditions.boundaries)

    PeriodicParticlesCells3D[Context](limitConditions, cellsMetadata, makeFlatCells(cellsMetadata, particles), minimumCellLength)
  }

  def makeEmptyFlatCells(cellsMetadata: ParticlesCells3DMetadata): List[List[Particle[Vector3D]]] = {
    val (layersNumber: Int, rowsNumber: Int, cellsNumber: Int) = cellsMetadata.cellsNumber

    (for {
      _ <- 1 to layersNumber
      _ <- 1 to rowsNumber
      _ <- 1 to cellsNumber
    } yield List[Particle[Vector3D]]()).toList
  }

  def makeFlatCells(cellsMetadata: ParticlesCells3DMetadata, particles: Seq[Particle[Vector3D]]): List[List[Particle[Vector3D]]] = {
    particles.foldLeft(makeEmptyFlatCells(cellsMetadata))(
      (accCells: List[List[Particle[Vector3D]]], particle: Particle[Vector3D]) => {
        (for {
          i <- cellsMetadata.getFlatCellIndexByCoordinate((particle.position.x, particle.position.y, particle.position.z))
        } yield (
          accCells.updated(i, accCells(i) :+ particle)
          )).getOrElse(accCells)
      }
    )
  }
}
