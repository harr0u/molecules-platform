package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{CutOffPotentialOptimization, LennardJonesPotential, PeriodicPotential}
import cats.Id
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, Square, RectangleFigure}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import simulation.state.ParticlesListState

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
//    No Reduce Optimization (ParticlesListState)
//    Uniform distributed on the start
class ValidationModelTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers with FrameLogTester {
  private def makePotential[V <: AlgebraicVector[V], Fig <: GeometricFigure](box: SpaceConditions[V, Fig]): LennardJonesPotential[V] = {
    new LennardJonesPotential[V]
      with PeriodicPotential[V, Fig]
    {
      override def limitConditions: SpaceConditions[V, Fig] = box
    }
  }

  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box)

      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, Id](
        ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        10E1.toInt,
        1,
      )

      energyError must be_<(10E-10)
    }

    "Save total energy when 36 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 6

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box)
      val particles = {
        val velocityFactor = 1.0
        ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, Id](
        particles,
        box,
        5E3.toInt,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2)
    }

    "Save total energy when 100 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10

      val box = {
        val boxWidth = particlesSideNumber / density
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box)
      val particles = {
        val velocityFactor = 1.3
        ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, Id](
        particles,
        box,
        1E3.toInt,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-5)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))

      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box)
      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, Id](
        ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, 1, velocityFactor)),
        box,
        100.toInt,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-10)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box)
      val particles = {
        val velocityFactor = 1.2
        ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, Id](
        particles,
        box,
        5E3.toInt,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2)

    }

    "Save total energy when 125 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 5

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box)

      val particles = {
        val velocityFactor = 1.9
        ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }


      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, Id](
        particles,
        box,
        5E3.toInt,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-3)
    }
  }
}
