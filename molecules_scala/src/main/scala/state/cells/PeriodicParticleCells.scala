package state.cells

import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector3D}
import state.ParticlesState
import state.cells.PeriodicFutureParticlesCells3D.{Cell, Cells}

import scala.collection.immutable.Seq

abstract class PeriodicParticleCells[V <: AlgebraicVector[V], F[_], Tuple[_]](
  cellsMetadata: ParticlesCellsMetadata[Tuple],
) extends ParticlesState[V, F] {

  def getAdjacentCells(flatIndex: Int): Seq[Seq[Particle[V]]]
  def getFlatCellIndexOfParticle(particle: Particle[V]): Option[Int]

  protected def mapCellWithIndex(cell: Seq[Particle[V]], index: Int, mapFn: (Particle[V]) => Particle[V]): (Seq[Particle[V]], Seq[(Int, Particle[V])]) = {
    cell.foldLeft((Seq[Particle[V]](), Seq[(Int, Particle[V])]()))((acc, particle) => {
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


  protected def sewMappedCells(computedFlatCells: Seq[(Seq[Particle[V]], Seq[(Int, Particle[V])])]): Vector[Seq[Particle[V]]]  = {
    val restParticlesFromCells: Vector[Seq[Particle[V]]] = computedFlatCells.map(_._1).toVector
    val particlesFromCellsThatLeft: Seq[Seq[(Int, Particle[V])]] = computedFlatCells.map(_._2)

    val newParticleCells: Vector[Seq[Particle[V]]] = particlesFromCellsThatLeft.foldLeft(restParticlesFromCells)((particlesAcc, leftParticlesFromCell) => {
      leftParticlesFromCell.foldLeft(particlesAcc)((particlesAcc1, leftParticleWithIndex) => {
        val (newCellIndex, leftParticle) = leftParticleWithIndex
        particlesAcc1.updated(newCellIndex, particlesAcc1(newCellIndex).appended(leftParticle))
      })
    })

    newParticleCells
  }

  protected def reduceCellWithIndex(cell: Seq[Particle[V]], index: Int, reduceFn: (Particle[V], Particle[V]) => (Particle[V])): Seq[Particle[V]] = {
    val cellsInvolvedInComputation: Seq[Seq[Particle[V]]] = getAdjacentCells(index)

    for ((particle) <- cell) yield {
      cellsInvolvedInComputation.foldLeft(particle)((accParticle, cell) => {
        cell.foldLeft(accParticle)((accParticle1, otherParticle) => {
          if (accParticle1.id != otherParticle.id) reduceFn(accParticle1, otherParticle) else accParticle1
        })
      })
    }
  }
}
