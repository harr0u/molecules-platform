package e2e

import calculation.space.SpaceConditions
import calculation.space.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.physics.potentials.{LennardJonesPotential, PeriodicPotential}
import cats.Id
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import simulation.frameLog.decorators.LeapFrogIteration
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
  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)

      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, Id](
        buildFrameLog(
          ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, 1, velocityFactor)),
          box
        ),
        1000,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)
      val particles = {
        val velocityFactor = 1.0
        ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)
      val particles = {
        val velocityFactor = 1.3
        ParticlesListState[Vector2D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
      )

      energyError must be_<(1E-5)
    }
  }
  
  "2D - Future" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))

      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, Future](
        buildFrameLog(
          ParticlesListState[Vector2D, Future](makeParticlesIn(box.boundaries, 1, velocityFactor)),
          box
        ),
        1000,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)
      val particles = {
        val velocityFactor = 1.0
        ParticlesListState[Vector2D, Future](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)
      val particles = {
        val velocityFactor = 1.3
        ParticlesListState[Vector2D, Future](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
      )

      energyError must be_<(1E-5).await(retries = 1, timeout = 120.seconds)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))

      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicPotential(box)
      val energyError: Double = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, Id](
        buildFrameLog(
          ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, 1, velocityFactor)),
          box,
          `∆t` = 0.0005
        ),
        100,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicPotential(box)
      val particles = {
        val velocityFactor = 1.2
        ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }

      val energyError: Double = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
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
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicPotential(box)

      val particles = {
        val velocityFactor = 1.9
        ParticlesListState[Vector3D, Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      }


      val energyError: Double = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
      )

      energyError must be_<(1E-3)
    }
  }
}
