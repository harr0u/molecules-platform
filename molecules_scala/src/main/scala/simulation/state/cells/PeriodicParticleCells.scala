package state.state.cells

import cats.{Applicative, Comonad, Eval, Monad, Parallel, Traverse}
import cats.implicits._
import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector3D}
import simulation.ParticlesState
import state.state.cells.PeriodicParticleCells.{ ListListTraverse, ListList}
import molecules.Utils._

import scala.collection.immutable.Seq

case class EscapedParticle[V <: AlgebraicVector[V]](particle: Particle[V], newCellIndex: Int)

abstract class PeriodicParticleCells[V <: AlgebraicVector[V], Context[_] : Monad, Tuple[_]]
  (implicit P : Parallel[Context]) extends ParticlesState[V, Context, ListList] {
  def cellsMetadata: ParticlesCellsMetadata[Tuple]

  def getAdjacentCells(flatIndex: Int): List[List[Particle[V]]]

  def getFlatCellIndexOfParticle(particle: Particle[V]): Option[Int]

  override def mapParticles(mapFn: Particle[V] => Particle[V]): Context[ParticlesState[V, Context, ListList]] = {
    counit.zipWithIndex.toList
      .parTraverse({ case (cell, index) => mapCellWithIndex(cell, index, mapFn) })
      .map(sewMappedCellsIntoFlatCells)
      .flatMap(updateWithParticles)
  }


  // DENGEROUS!!! There is no change position check for particles
  override def mapParticlesPairs(mapFn: (Particle[V], ParticlesState[V, Context, ListList]) => Particle[V]): Context[ParticlesState[V, Context, ListList]] = {
    counit.zipWithIndex.toList
      .parTraverse({ case (cell, index) => updateWithParticles(getAdjacentCells(index))
        .map(state =>
          cell.map(particle => mapFn(particle, state)))})
      .flatMap(updateWithParticles)
  }

  protected def mapCellWithIndex(cell: List[Particle[V]], index: Int, mapFn: (Particle[V]) => Particle[V]): Context[(List[Particle[V]], List[EscapedParticle[V]])] = {
    Context.pure {
      cell.foldLeft((List[Particle[V]](), List[EscapedParticle[V]]()))((acc, particle) => {
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


  protected def sewMappedCellsIntoFlatCells(computedFlatCells: List[(List[Particle[V]], List[EscapedParticle[V]])]): ListList[Particle[V]]  = {
    val currentCellsWithoutEscapedParticles : ListList[Particle[V]] = computedFlatCells.map(_._1)
    val escapedParticles: Seq[Seq[EscapedParticle[V]]] = computedFlatCells.map(_._2)

    escapedParticles.foldLeft(currentCellsWithoutEscapedParticles)(
      (particlesAcc, escaped) => {
        escaped.foldLeft(particlesAcc)((particlesAcc1, p: EscapedParticle[V]) => {
          particlesAcc1.updated(p.newCellIndex, particlesAcc1(p.newCellIndex) :+ p.particle)
        })
      }
    )
  }

  protected def getPeriodicAdjIndex(index: Int, number: Int): Int = {
    val index_ = index % number
    if (index_ < 0) number + index_ else index_
  }
}

object PeriodicParticleCells {
  type ListList[T] = List[List[T]]
  type Cell[V <: AlgebraicVector[V]] = Seq[Particle[V]]
  def Cell[V <: AlgebraicVector[V]](): Seq[Particle[V]] = Seq[Particle[V]]()

  type Cells[V <: AlgebraicVector[V]] = Seq[Seq[Particle[V]]]
  def Cells[V <: AlgebraicVector[V]](): ListList[Particle[V]] = List[List[Particle[V]]]()

  implicit val ListListTraverse: Traverse[ListList] = new Traverse[ListList] {
    lazy val traverseForList: Traverse[List] = implicitly[Traverse[List]]

    override def foldLeft[A, B](fa: ListList[A], b: B)(f: (B, A) => B): B = {
      traverseForList.foldLeft(fa, b)((acc: B, la: List[A]) => {
        traverseForList.foldLeft(la, acc)(f)
      })
    }

    override def foldRight[A, B](fa: ListList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      traverseForList.foldRight(fa, lb)((la: List[A], acc: Eval[B]) => {
        traverseForList.foldRight(la, acc)(f)
      })
    }

    override def traverse[G[_], A, B](fa: ListList[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[ListList[B]] = {
      traverseForList.traverse(fa)(_.traverse(f))
    }
  }
}
