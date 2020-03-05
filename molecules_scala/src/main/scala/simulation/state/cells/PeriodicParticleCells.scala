package state.state.cells

import cats.{Comonad, Monad, Parallel, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector3D}
import simulation.ParticlesState
import state.state.cells.PeriodicParticleCells.{Cell, Cells, SeqSeq}

import scala.collection.immutable.Seq

case class EscapedParticle[V <: AlgebraicVector[V]](particle: Particle[V], newCellIndex: Int)

abstract class PeriodicParticleCells[V <: AlgebraicVector[V], Context[_] : Monad, Tuple[_]](implicit par : Parallel[Context])
  extends ParticlesState[V, Context, SeqSeq] {
  def cellsMetadata: ParticlesCellsMetadata[Tuple]

  def currentFlatCells: Cells[V]

  def getAdjacentCells(flatIndex: Int): Seq[Cell[V]]

  def getFlatCellIndexOfParticle(particle: Particle[V]): Option[Int]

  def updateWithFlatCells(currentFlatCells: Cells[V]): PeriodicParticleCells[V, Context, Tuple]

  val Context: Monad[Context] = implicitly[Monad[Context]]

  override def getParticles: SeqSeq[Particle[V]] = currentFlatCells

  override def mapParticles(mapFn: Particle[V] => Particle[V]): Context[ParticlesState[V, Context, SeqSeq]] = {
    mapWithIndex(mapCellWithIndex(_, _, mapFn), sewMappedCellsIntoFlatCells)
  }

  override def mapParticlesPairs(mapFn: (Particle[V], ParticlesState[V, Context, SeqSeq]) => Particle[V]): Context[ParticlesState[V, Context, SeqSeq]] = {

  }

  protected def mapWithIndex[T](mapiCellFn: (Cell[V], Int) => Context[T], beforeUpdate: List[T] => Cells[V]): Context[ParticlesState[V, Context, SeqSeq]] = {
    currentFlatCells.zipWithIndex.toList
      .parTraverse({ case (cell, index) => mapiCellFn(cell, index) })
      .map(beforeUpdate andThen this.updateWithFlatCells)
  }

  protected def mapCellWithIndex(cell: Cell[V], index: Int, mapFn: (Particle[V]) => Particle[V]): Context[(Cell[V], Seq[EscapedParticle[V]])] = {
    Context.pure {
      cell.foldLeft((Cell[V](), Seq[EscapedParticle[V]]()))((acc, particle) => {
        val (rest, left) = acc

        val newParticle = mapFn(particle)
        if (newParticle.position == particle.position) {
          (rest :+ newParticle, left)
        } else {
          getFlatCellIndexOfParticle(newParticle)
            .withFilter(newIndex => newIndex != index)
            .map(newIndex => (rest, left :+ EscapedParticle(newParticle, newIndex)))
            .getOrElse((rest :+ newParticle, left))
        }
      })
    }
  }


  protected def sewMappedCellsIntoFlatCells(computedFlatCells: List[(Cell[V], Seq[EscapedParticle[V]])]): Cells[V]  = {
    val currentCellsWithoutLeftParticles : Cells[V] = computedFlatCells.map(_._1)
    val escapedParticles: Seq[Seq[EscapedParticle[V]]] = computedFlatCells.map(_._2)

    escapedParticles.foldLeft(currentCellsWithoutLeftParticles)(
      (particlesAcc, leftParticlesFromCell) => {
        leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, p: EscapedParticle[V]) => {
          particlesAcc1.updated(p.newCellIndex, particlesAcc1(p.newCellIndex) :+ p.particle)
        })
      }
    )
  }

  protected def reduceCellWithIndex(cell: Cell[V], index: Int, reduceFn: (Particle[V], Particle[V]) => (Particle[V])): Context[Cell[V]] = {
    Context.pure {
      val cellsInvolvedInComputation: Seq[Cell[V]] = getAdjacentCells(index)

      for ((particle) <- cell) yield {
        cellsInvolvedInComputation.foldLeft(particle)((accParticle, cell) => {
          cell.foldLeft(accParticle)((accParticle1, otherParticle) => {
            if (accParticle1.id != otherParticle.id) reduceFn(accParticle1, otherParticle) else accParticle1
          })
        })
      }
    }
  }

  protected def getPeriodicAdjIndex(index: Int, number: Int): Int = {
    val index_ = index % number
    if (index_ < 0) number + index_ else index_
  }
}

object PeriodicParticleCells {
  type SeqSeq[T] = Seq[Seq[T]]
  type Cell[V <: AlgebraicVector[V]] = Seq[Particle[V]]
  def Cell[V <: AlgebraicVector[V]](): Cell[V] = Seq[Particle[V]]()

  type Cells[V <: AlgebraicVector[V]] = Seq[Cell[V]]
  def Cells[V <: AlgebraicVector[V]](): Cells[V] = Seq[Cell[V]]()
}
