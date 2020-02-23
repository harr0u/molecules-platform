package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesCutOffPotential, LennardJonesPotential, PotentialCalculator}
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


class CellsTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers with FrameLogTester {
  sequential

  "2D" >> {
    implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesCutOffPotential[Vector2D]()
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val rCutOff = 5.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, 1, velocityFactor), box, rCutOff),
        box,
        1E4.toInt,
        1,
        10E-10
      )xk
    }

    "Save total energy when 15*15=225 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 15

      val box: RectanglePeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        println(s"boxWidth ~> $boxWidth")
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles: PeriodicFutureParticlesCells2D = {
        val velocityFactor = 1.2
        val rCutOff = 5.0
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)      }

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        5E2.toInt,
        225,
        1E-4,
        0.001,
        verbose = Some(100)
      )
    }

    "Save total energy when 30*30=900 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 30

      val box = {
        val boxWidth = particlesSideNumber / density
        println(boxWidth)
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      val particles = {
        val velocityFactor = 13.0
        val rCutOff = 5.0
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      matchMeanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        1E3.toInt,
        900,
        1E-5,
        0.0005,
        verbose = Some(100)
      )
    }
  }


  "3D" >> {
    implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesCutOffPotential[Vector3D]()
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val rCutOff = 5.0

      matchMeanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, 1, velocityFactor), box, rCutOff),
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
      val particles: PeriodicFutureParticlesCells3D = {
        val velocityFactor = 12.0
        val rCutOff = 5.0
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
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
      val particles: PeriodicFutureParticlesCells3D = {
        val velocityFactor = 12.0
        val rCutOff = 5.0
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }


      matchMeanSquaredErrorOfTotalEnergy(
        particles,
        box,
        5E3.toInt,
        125,
        1E-4,
        0.0005,
      )
    }
  }
}
