package state.state.cells

import cats.{Monad, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector3D}
import simulation.ParticlesState
import state.state.cells.PeriodicParticleCells.{Cell, Cells}

import scala.collection.immutable.Seq

case class EscapedParticle[V <: AlgebraicVector[V]](particle: Particle[V], newCellIndex: Int)

abstract class PeriodicParticleCells[V <: AlgebraicVector[V], F[_] : Monad, Tuple[_]](implicit SeqForF : Traverse[List]) extends ParticlesState[V, F] {
  def cellsMetadata: ParticlesCellsMetadata[Tuple]
  def currentFlatCells: Cells[V]
  def getAdjacentCells(flatIndex: Int): Seq[Cell[V]]
  def getFlatCellIndexOfParticle(particle: Particle[V]): Option[Int]
  def updateWithFlatCells(currentFlatCells: Cells[V]): PeriodicParticleCells[V, F, Tuple]

  val F = implicitly[Monad[F]]

  override def getParticles: List[Particle[V]] = currentFlatCells.reduce(_ ++ _).to(List)

  override def map(mapFn: Particle[V] => Particle[V]): F[ParticlesState[V, F]] = {
    val mapFs: List[F[(Cell[V], Seq[EscapedParticle[V]])]] = for {
      (cell, index) <- currentFlatCells.zipWithIndex.toList
    } yield {
      F.pure {
        mapCellWithIndex(cell, index, mapFn)
      }
    }
    for (computedRawFlatCells <- SeqForF.sequence(mapFs)) yield {
      this.updateWithFlatCells(sewMappedCellsIntoFlatCells(computedRawFlatCells))
    }
  }

  override def reduce(reduceFn: (Particle[V], Particle[V]) => Particle[V]): F[ParticlesState[V, F]] = {
    val reduceFs: List[F[Cell[V]]] = for {
      (cell, index) <- currentFlatCells.zipWithIndex.toList
    } yield {
      F.pure {
        reduceCellWithIndex(cell, index, reduceFn)
      }
    }

    SeqForF.sequence(reduceFs).map(newFlatCells=> this.updateWithFlatCells(newFlatCells))
  }

  protected def mapCellWithIndex(cell: Cell[V], index: Int, mapFn: (Particle[V]) => Particle[V]): (Cell[V], Seq[EscapedParticle[V]]) = {
    cell.foldLeft((Cell[V](), Seq[EscapedParticle[V]]()))((acc, particle) => {
      val (rest, left) = acc

      val newParticle = mapFn(particle)
      if (newParticle.position == particle.position) {
        (rest.appended(newParticle), left)
      } else {
        getFlatCellIndexOfParticle(newParticle)
          .withFilter(newIndex => newIndex != index)
          .map(newIndex => (rest, left.appended(EscapedParticle(newParticle, newIndex))))
          .getOrElse((rest.appended(newParticle), left))
      }
    })
  }


  protected def sewMappedCellsIntoFlatCells(computedFlatCells: Seq[(Cell[V], Seq[EscapedParticle[V]])]): Cells[V]  = {
    val currentCellsWithoutLeftParticles : Cells[V] = computedFlatCells.map(_._1).toVector
    val escapedParticles: Seq[Seq[EscapedParticle[V]]] = computedFlatCells.map(_._2)

    escapedParticles.foldLeft(currentCellsWithoutLeftParticles)(
      (particlesAcc, leftParticlesFromCell) => {
        leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, p: EscapedParticle[V]) => {
          particlesAcc1.updated(p.newCellIndex, particlesAcc1(p.newCellIndex).appended(p.particle))
        })
      }
    )
  }

  protected def reduceCellWithIndex(cell: Cell[V], index: Int, reduceFn: (Particle[V], Particle[V]) => (Particle[V])): Cell[V] = {
    val cellsInvolvedInComputation: Seq[Cell[V]] = getAdjacentCells(index)

    for ((particle) <- cell) yield {
      cellsInvolvedInComputation.foldLeft(particle)((accParticle, cell) => {
        cell.foldLeft(accParticle)((accParticle1, otherParticle) => {
          if (accParticle1.id != otherParticle.id) reduceFn(accParticle1, otherParticle) else accParticle1
        })
      })
    }
  }

  protected def getPeriodicAdjIndex(index: Int, number: Int): Int = {
    val index_ = index % number
    if (index_ < 0) number + index_ else index_
  }
}

object PeriodicParticleCells {
  type Cell[V <: AlgebraicVector[V]] = Seq[Particle[V]]
  def Cell[V <: AlgebraicVector[V]](): Cell[V] = Seq[Particle[V]]()

  type Cells[V <: AlgebraicVector[V]] = Seq[Cell[V]]
  def Cells[V <: AlgebraicVector[V]](): Cells[V] = Seq[Cell[V]]()
}
