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

// Validation model is the most accurate (-> slowest) model
//    Lennard Jones Potential
//    Leap Frog Integration algorithm
//    Periodic Cube (Square)
//    No Reduce Optimization (ParticlesSeqState)
//    Uniform distributed on the start
class ValidationModelTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers with FrameLogTester {
  "2D" >> {
    implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPotential()
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        ParticlesSeqState(makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        10E1.toInt,
        1,
        10E-10
      )
    }

    "Save total energy when 36 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 6

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles = {
        val velocityFactor = 1.0
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        1E4.toInt,
        36,
        1E-2,
        `∆t` = 0.0005,
      )

    }

    "Save total energy when 100 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles = {
        val velocityFactor = 1.3
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        1E4.toInt,
        100,
        1E-5,
        0.0005,
      )
    }
  }


  "3D" >> {
    implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPotential()
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))

      matchMeanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        ParticlesSeqState(makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        10E1.toInt,
        1,
        10E-10
      )
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 1.2
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      matchMeanSquaredErrorOfTotalEnergy(
        particles,
        box,
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
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      val particles: ParticlesSeqState[Vector3D] = {
        val velocityFactor = 1.9
        ParticlesSeqState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }


      matchMeanSquaredErrorOfTotalEnergy(
        particles,
        box,
        5E3.toInt,
        125,
        5E-3,
        0.001,
      )
    }
  }
}
