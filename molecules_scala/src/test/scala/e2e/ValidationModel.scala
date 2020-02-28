package e2e

import calculation.space.SpaceConditions
import calculation.space.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.physics.potentials.{LennardJonesPotential, PeriodicPotential}
import cats.{Id, Monad, Parallel}
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import simulation.ParticlesState
import simulation.frameLog.decorators.LeapFrogIteration
import simulation.state.ParticlesListState
import FrameLogTester._
import calculation.physics.Utils

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
class ValidationModelTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers {
  def energyErrorOfValidation2DSimulation[M[_] : Monad](
    density: Double,
    particlesSideNumber: Int,
    velocityFactor: Double,
    numberOfFrames: Int,
    `∆t`: Double = 0.0001): Option[M[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector2D, RectangleFigure] = RectanglePeriodicSpaceConditions(Square(boxWidth))
      val particlesState: ParticlesState[Vector2D, M] = ParticlesListState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicPotential(box)

      meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, M](
        buildFrameLog(particlesState, box),
        numberOfFrames,
      )
    }
  }

  def energyErrorOfValidation3DSimulation[M[_] : Monad](
                                                         density: Double,
                                                         particlesSideNumber: Int,
                                                         velocityFactor: Double,
                                                         numberOfFrames: Int,
                                                         `∆t`: Double = 0.0001): Option[M[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector3D, CubicFigure] = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val particlesState: ParticlesState[Vector3D, M] = ParticlesListState(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicPotential(box)

      meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, M](
        buildFrameLog(particlesState, box),
        numberOfFrames,
      )
    }
  }


  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val energyError: Option[Double] = energyErrorOfValidation2DSimulation[Id](
        0.05,
        1,
        6.66,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-10)
    }

    "Save total energy when 36 molecules are presented with density=0.7" >> {
      val energyError: Option[Double] = energyErrorOfValidation2DSimulation[Id](
        0.7,
        6,
        1.24,
        5000,
        0.0005
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 100 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfValidation2DSimulation[Id](
        0.8,
        10,
        1.24,
        500,
        0.0005
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }
  }

  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val energyError: Option[Double] = energyErrorOfValidation3DSimulation[Id](
        0.05,
        1,
        6.66,
        100,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-10)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val energyError: Option[Double] = energyErrorOfValidation2DSimulation[Id](
        0.7,
        3,
        1.24,
        10000,
        0.0005
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 125 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfValidation2DSimulation[Id](
        0.8,
        3,
        1.24,
        50000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }
  }
}
