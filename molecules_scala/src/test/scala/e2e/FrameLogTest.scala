package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesPotential, PotentialCalculator}
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import state.{ParticlesSeqState, ParticlesState, ParticlesStateReducer}
import state.cells.{PeriodicFutureParticlesCells2D, PeriodicFutureParticlesCells3D}

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations



trait FrameLogTester {
  this: mutable.Specification with FutureMatchers =>
  def matchMeanSquaredErrorOfTotalEnergy[V <: AlgebraicVector[V], Fig <: GeometricFigure](
                                                                                     particles: ParticlesState[V, Future],
                                                                                     box: SpaceConditions[V, Fig],
                                                                                     numberOfFrames: Int,
                                                                                     expectedNumberOfParticles: Int,
                                                                                     meanSquaredErrorLimit: Double,
                                                                                     `∆t`: Double = 0.001,
                                                                                     verbose: Option[Int] = None
                                                                                   )(implicit ee: ExecutionEnv, potential: PotentialCalculator[V]): MatchResult[Future[Double]] = {
    val leapFrogIteration: LeapFrogIteration[V, Fig] = LeapFrogIteration(particles, box, `∆t` = `∆t`)

    leapFrogIteration.particles.counit.length must_=== expectedNumberOfParticles

    val particlesLog = LazyList.iterate(leapFrogIteration.init())(iF => iF.flatMap(iteration => iteration.iterationStep()))

    val framesHistory: LazyList[Future[(Double, Long)]] = buildTotalEnergyLog(particlesLog, verbose)

    val meanSquaredError: Future[Double] = for {
      framesEnergy: LazyList[(Double, Long)] <- Future.sequence(framesHistory.take(numberOfFrames))
    } yield {
      val energySeq: Seq[Double] = framesEnergy.map(_._1)
      val average = energySeq.iterator.sum / numberOfFrames

      energySeq.map(e => Math.pow(e - average, 2)).iterator.sum / numberOfFrames
    }

    meanSquaredError must be_<(meanSquaredErrorLimit).await(retries = 1, timeout = 120.seconds)
  }

  def buildTotalEnergyLog[V <: AlgebraicVector[V], F <: GeometricFigure](moleculesLog: LazyList[Future[LeapFrogIteration[V, F]]], verbose: Option[Int] = None)
                                                                        (implicit ec: ExecutionContext): LazyList[Future[(Double, Long)]] = {
    val log = (index: Int, energies: (Double, Double, Double)) => if (verbose.exists(x => index % x == 0)) {
      val (total, potential, kineticEnergy) = energies
      println(f"Frame ${index} - T[${total}] - P[${potential}] - K[${kineticEnergy}]")
    }

    moleculesLog.zipWithIndex.map {
      case (iter, i) => iter.map((iteration) => {
        val numberOfParticles = iteration.particles.counit.length
        val kineticEnergy: Double = iteration.particles.counit.map((p) => p.velocity.squaredLength / 2).iterator.sum / numberOfParticles
        val potential: Double = iteration.particles.counit.map(_.potential).iterator.sum / numberOfParticles
        val total: Double = kineticEnergy + potential

        log(i, (total, potential, kineticEnergy))
        (total, System.currentTimeMillis)
      })
    }
  }

  def makeParticlesIn(box: CubicFigure, sideNumber: Int, velocityFactor: Double): Seq[Particle[Vector3D]] = {
    val rng = new Random(0L)

    for {
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
  }

  def makeParticlesIn(box: RectangleFigure, sideNumber: Int, velocityFactor: Double): Seq[Particle[Vector2D]] = {
    val rng = new Random(0L)

    for {
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
  }

}
