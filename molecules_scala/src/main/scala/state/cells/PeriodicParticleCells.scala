package state.cells

import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector3D}
import state.ParticlesState
import state.cells.PeriodicParticleCells.{Cell, Cells}

import scala.collection.immutable.Seq

abstract class PeriodicParticleCells[V <: AlgebraicVector[V], F[_], Tuple[_]](
                                                                               cellsMetadata: ParticlesCellsMetadata[Tuple],
                                                                             ) extends ParticlesState[V, F] {

  def getAdjacentCells(flatIndex: Int): Seq[Cell[V]]
  def getFlatCellIndexOfParticle(particle: Particle[V]): Option[Int]

  protected def mapCellWithIndex(cell: Cell[V], index: Int, mapFn: (Particle[V]) => Particle[V]): (Cell[V], Seq[(Int, Particle[V])]) = {
    cell.foldLeft((Cell[V](), Seq[(Int, Particle[V])]()))((acc, particle) => {
      val (rest, left) = acc

      val newParticle = mapFn(particle)
      if (newParticle.position == particle.position) {
        (rest.appended(newParticle), left)
      } else {
        (for {
          newIndex <- getFlatCellIndexOfParticle(newParticle)
          if (newIndex) != index
        } yield {
          (rest, left.appended((newIndex, newParticle)))
        }) getOrElse {
          (rest.appended(newParticle), left)
        }
      }
    })
  }


  protected def sewMappedCellsIntoFlatCells(computedFlatCells: Seq[(Cell[V], Seq[(Int, Particle[V])])]): Cells[V]  = {
    val currentCellsWithoutLeftParticles : Cells[V] = computedFlatCells.map(_._1).toVector
    val particlesFromCellsThatLeft: Seq[Seq[(Int, Particle[V])]] = computedFlatCells.map(_._2)

    particlesFromCellsThatLeft.foldLeft(currentCellsWithoutLeftParticles)(
      (particlesAcc, leftParticlesFromCell) => {
        leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, leftParticleWithIndex) => {
          val (newCellIndex, leftParticle) = leftParticleWithIndex
          particlesAcc1.updated(newCellIndex, particlesAcc1(newCellIndex).appended(leftParticle))
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

  protected def getPeriodicAdjIndex(index: Int, delta: Int, number: Int): Int = {
    val index_ = (index + delta) % number
    if (index_ < 0) number + index_ else index_
  }
}

object PeriodicParticleCells {
  type Cell[V <: AlgebraicVector[V]] = Seq[Particle[V]]
  def Cell[V <: AlgebraicVector[V]](): Cell[V] = Seq[Particle[V]]()

  type Cells[V <: AlgebraicVector[V]] = Seq[Cell[V]]
  def Cells[V <: AlgebraicVector[V]](): Cells[V] = Seq[Cell[V]]()
}
