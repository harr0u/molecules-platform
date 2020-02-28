package e2e

import calculation.physics.Utils
import calculation.physics.potentials.{CutOffPotentialOptimization, LennardJonesPotential, PeriodicPotential}
import calculation.space.SpaceConditions
import calculation.space.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import cats.{Id, Monad, Parallel}
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import e2e.FrameLogTester._
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import simulation.ParticlesState
import simulation.state.ParticlesListState
import state.state.cells.{PeriodicParticlesCells2D, PeriodicParticlesCells3D}

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations
import cats.implicits._

class CellsTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers {
  def energyErrorOfCells2DSimulation[M[_] : Monad](
                                                         density: Double,
                                                         particlesSideNumber: Int,
                                                         velocityFactor: Double,
                                                         numberOfFrames: Int,
                                                         `∆t`: Double = 0.0001,
                                                         rCutOff: Double = 5.0)(implicit P: Parallel[M]): Option[M[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector2D, RectangleFigure] = RectanglePeriodicSpaceConditions(Square(boxWidth))
      val particlesState: ParticlesState[Vector2D, M] = PeriodicParticlesCells2D.create[M](
        makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor),
        box,
        rCutOff
      )
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicCutOffPotential(box)(rCutOff)

      meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure, M](
        buildFrameLog(particlesState, box),
        numberOfFrames,
      )
    }
  }

  def energyErrorOfCells3DSimulation[M[_] : Monad](
                                                         density: Double,
                                                         particlesSideNumber: Int,
                                                         velocityFactor: Double,
                                                         numberOfFrames: Int,
                                                         `∆t`: Double = 0.0001,
                                                         rCutOff: Double = 5.0)(implicit par : Parallel[M]): Option[M[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector3D, CubicFigure] = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val particlesState: ParticlesState[Vector3D, M] = PeriodicParticlesCells3D.create[M](
        makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor),
        box,
        rCutOff
      )
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicCutOffPotential(box)(rCutOff)

      meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, M](
        buildFrameLog(particlesState, box),
        numberOfFrames,
      )
    }
  }

  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.05,
        1,
        6.66,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-10)
    }

    "Save total energy when 15*15=225 molecules are presented with density=0.7" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.7,
        15,
        1.24,
        500,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 25*25=625 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.8,
        25,
        1.24,
        500,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 50*50=2500 molecules are presented with density=0.6" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.6,
        50,
        1.24,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 100*100=10000 molecules are presented with density=0.65" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.65,
        100,
        1.24,
        250,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }
  }


  "2D - Parallel" >> {
    "Save total energy when 1 molecule is presented" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.05,
        1,
        6.66,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-10)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val energyError: Option[Double] = energyErrorOfCells3DSimulation[Id](
        0.03,
        1,
        1.24,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-8)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.8,
        3,
        1.24,
        500,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 1000 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.8,
        10,
        1.24,
        500,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 125000 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.8,
        50,
        1.24,
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }
  }
}
