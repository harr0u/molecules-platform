package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesPeriodicCutOffPotential, LennardJonesPeriodicPotential, LennardJonesPotential, PotentialCalculator}
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
import cats.implicits._
import org.specs2.specification.AllExpectations

// Validation model is the most accurate (-> slowest) model
//    Lennard Jones Potential
//    Leap Frog Integration algorithm
//    Periodic Cube (Square)
//    No Reduce Optimization (ParticlesSeqState)
//    Uniform distributed on the start
class ValidationModelTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers with FrameLogTester {
  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicPotential[Vector2D, RectangleFigure](box)

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        ParticlesSeqState(makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        10E1.toInt,
        1,
      )

      energyError must be_<(10E-10).await(retries = 1, timeout = 120.seconds)
    }

    "Save total energy when 36 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 6

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicPotential[Vector2D, RectangleFigure](box)
      val particles = {
        val velocityFactor = 1.0
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        5E3.toInt,
        36,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)
    }

    "Save total energy when 100 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicPotential[Vector2D, RectangleFigure](box)
      val particles = {
        val velocityFactor = 1.3
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        1E3.toInt,
        100,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-5).await(1, 30.seconds)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))
      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicPotential[Vector3D, CubicFigure](box)

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        ParticlesSeqState(makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        100.toInt,
        1,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-10).await(retries = 1, timeout = 120.seconds)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicPotential[Vector3D, CubicFigure](box)
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 1.2
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        particles,
        box,
        5E3.toInt,
        27,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)

    }

    "Save total energy when 125 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 5

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicPotential[Vector3D, CubicFigure](box)
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 1.9
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }


      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        particles,
        box,
        5E3.toInt,
        125,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-3).await(retries = 1, timeout = 120.seconds)
    }
  }
}
