package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesCutOffPotential
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

// Validation model is the most accurate (-> slowest) model
//    Lennard Jones Potential
//    Leap Frog Integration algorithm
//    Periodic Cube (Square)
//    No Reduce Optimization (ParticlesSeqState)
//    Uniform distributed on the start
class ValidationModelTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers {
  sequential

  private def buildValidationModelFrameLog[V <: AlgebraicVector[V], Fig <: GeometricFigure](
                                              box: SpaceConditions[V, Fig],
                                              particles: ParticlesState[V, Future],
                                              numberOfFrames: Int,
                                              expectedNumberOfParticles: Int,
                                              meanSquaredErrorLimit: Double,
                                              `∆t`: Double = 0.001,
                                              verbose: Option[Int] = None
                                            ): MatchResult[Future[Double]] = {
    val leapFrogIteration: LeapFrogIteration[V, Fig] = LeapFrogIteration(particles, box, `∆t` = `∆t`)
    leapFrogIteration.particles.counit.length must_=== expectedNumberOfParticles


    val particlesLog = LazyList.iterate(leapFrogIteration.init())(
      iter => iter.flatMap(iteration => iteration.iterationStep())
    )

    val framesHistory: LazyList[Future[(Double, Long)]] = ValidationModelTest.buildTotalEnergyLog(particlesLog, verbose)

    val meanSquaredError: Future[Double] = for {
      framesEnergy: LazyList[(Double, Long)] <- Future.sequence(framesHistory.take(numberOfFrames))
    } yield {
      val energySeq: Seq[Double] = framesEnergy.map(_._1)
      val average = energySeq.iterator.sum / numberOfFrames

      energySeq.map(e => Math.pow(e - average, 2)).iterator.sum / numberOfFrames
    }

    meanSquaredError must be_<(meanSquaredErrorLimit).await(retries = 1, timeout = 120.seconds)
  }

  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      buildValidationModelFrameLog[Vector2D, RectangleFigure](
        box,
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, 1, velocityFactor)),
        10E1.toInt,
        1,
        10E-10
      )
    }

    "Save total energy when 36 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 6

      val box = {
        val boxWidth = particlesSideNumber * density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles = {
        val velocityFactor = 10.0
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      buildValidationModelFrameLog[Vector2D, RectangleFigure](
        box,
        particles,
        5E3.toInt,
        36,
        1
      )

    }

    "Save total energy when 100 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10

      val box = {
        val boxWidth = particlesSideNumber * density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles = {
        val velocityFactor = 13.0
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      buildValidationModelFrameLog[Vector2D, RectangleFigure](
        box,
        particles,
        1E4.toInt,
        100,
        1E-5,
        0.0005,
      )
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))

      buildValidationModelFrameLog[Vector3D, CubicFigure](
        box,
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, 1, velocityFactor)),
        10E1.toInt,
        1,
        10E-10
      )
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber * density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 12.0
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      buildValidationModelFrameLog(
        box,
        particles,
        5E4.toInt,
        27,
        1E-3,
        0.0001,
      )

    }

    "Save total energy when 125 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 5

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber * density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 9.0
        ParticlesSeqState(ValidationModelTest.makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }


      buildValidationModelFrameLog(
        box,
        particles,
        5E3.toInt,
        125,
        1E-4,
        0.0005,
      )
    }
  }
}

object ValidationModelTest {
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
        Vector3D(rng.nextDouble(), rng.nextDouble(), rng.nextDouble()) * velocityFactor,
        Vector3D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }
  }

  def makeParticlesIn(box: RectangleFigure, sideNumber: Int, velocityFactor: Double): Seq[Particle[Vector2D]] = {
    val rng = new Random(0L)
    val vel = Vector2D(rng.nextDouble(), rng.nextDouble())

    for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
    } yield {
      new Particle[Vector2D](
        i * sideNumber + j,
        Vector2D((i + 0.5) * box.width / sideNumber, (j + 0.5) * box.length / sideNumber),
        vel * velocityFactor,
        Vector2D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }
  }
}