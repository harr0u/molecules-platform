package e2e

import calculation.physics.potentials.PairwisePotentialCalculator
import calculation.space.SpaceConditions
import cats.{Functor, Monad, Traverse}
import domain.Particle
import domain.geometry.figures.{CubicFigure, GeometricFigure, RectangleFigure}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import simulation.ParticlesState
import simulation.frameLog.{FrameLog, PeriodicLennardJonesFrameLog}
import simulation.reducers.ParticlesStateReducer
import cats.implicits._

import scala.util.Random


object FrameLogTester {
  def buildPeriodicLJFrameLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, Context[_] : Monad, T[_] : Traverse](
    particles: ParticlesState[V, Context, T],
    box: SpaceConditions[V, Fig],
    `∆t`: Double = 0.0001
)(implicit pc: PairwisePotentialCalculator[V]): PeriodicLennardJonesFrameLog[V, Fig, Context, T] = {
    PeriodicLennardJonesFrameLog(particles, new ParticlesStateReducer[V, Context, T], box, `∆t`)
  }

  def meanSquaredErrorOfTotalEnergy[V <: AlgebraicVector[V], Context[_] : Monad, T[_] : Traverse, FL <: FrameLog[V, Context, T, FL]](
    frameLog: FrameLog[V, Context, T, FL],
    numberOfFrames : Int,
    verbose: Option[Int] = Some(100)
  ): Context[Double] = {

    val particlesLog: LazyList[Context[FL]] = LazyList.iterate(frameLog.init, numberOfFrames)(
      iF => iF.flatMap(iteration => iteration.next)
    )

    val log = (index: Int, energies: (Double, Double, Double)) => if (verbose.exists(x => index % x == 0)) {
      val (total, potential, kineticEnergy) = energies
      println(f"Frame ${index} - T[${total}] - P[${potential}] - K[${kineticEnergy}]")
    }

    val framesHistory: Context[List[(Double, Long)]] = particlesLog.zipWithIndex
      .map {
        case (iter, i: Int) => iter.map((iteration) => {
          val numberOfParticles = iteration.particles.getParticles.length
          val kineticEnergy: Double = iteration.particles.getParticles.map((p) => p.velocity.squaredLength / 2).iterator.sum / numberOfParticles
          val potential: Double = iteration.particles.getParticles.map(_.potential).iterator.sum / numberOfParticles
          val total: Double = kineticEnergy + potential

          log(i, (total, potential, kineticEnergy))

          (total, System.currentTimeMillis)
        })
      }
      .toList.sequence

    framesHistory.map(framesEnergy => {
      val energySeq: List[Double] = framesEnergy.map(_._1)
      val average = energySeq.sum / numberOfFrames

      energySeq.map(e => Math.pow(e - average, 2)).sum / numberOfFrames
    })
  }

  def makeParticlesIn(box: CubicFigure, sideNumber: Int, velocityFactor: Double): List[Particle[Vector3D]] = {
    val rng = new Random(0L)

    val particlesSeq = for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
      k <- 0 until sideNumber
    } yield {
      new Particle[Vector3D](
        i * sideNumber * sideNumber + j * sideNumber + k,
        Vector3D((i + 0.5) * box.width / sideNumber, (j + 0.5) * box.width / sideNumber, (k + 0.5) * box.width / sideNumber),
        Vector3D(rng.nextDouble() - 0.5, rng.nextDouble() - 0.5, rng.nextDouble() - 0.5) * velocityFactor,
        Vector3D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }

    particlesSeq.toList
  }

  def makeParticlesIn(box: RectangleFigure, sideNumber: Int, velocityFactor: Double): List[Particle[Vector2D]] = {
    val rng = new Random(0L)

    val particlesSeq = for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
    } yield {
      new Particle[Vector2D](
        i * sideNumber + j,
        Vector2D((i + 0.5) * box.width / sideNumber, (j + 0.5) * box.length / sideNumber),
        Vector2D(rng.nextDouble() - 0.5, rng.nextDouble() - 0.5) * velocityFactor,
        Vector2D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }

    particlesSeq.toList
  }
}
