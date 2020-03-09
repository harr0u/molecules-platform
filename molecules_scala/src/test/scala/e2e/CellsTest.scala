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
import simulation.frameLog.PeriodicLennardJonesFrameLog
import simulation.reducers.ParticlesStateReducer
import state.state.cells.PeriodicParticlesCells.ListList
import state.state.cells.{PeriodicParticlesCells2D, PeriodicParticlesCells3D}
import state.state.cells.PeriodicParticlesCells.ListListTraverse

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations
import cats.implicits._

class CellsTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers {
  def energyErrorOfCells2DSimulation[Context[_] : Monad](
                                                         density: Double,
                                                         particlesSideNumber: Int,
                                                         velocityFactor: Double,
                                                         numberOfFrames: Int,
                                                         `∆t`: Double = 0.0001,
                                                         rCutOff: Double = 5.0)(implicit P: Parallel[Context]): Option[Context[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector2D, RectangleFigure] = RectanglePeriodicSpaceConditions(Square(boxWidth))
      val particlesState: ParticlesState[Vector2D, Context, ListList] = {
        PeriodicParticlesCells2D.create[Context](
          makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor),
          box,
          rCutOff
        )
      }

      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = LennardJonesPotential.periodicCutOffPotential(box)(rCutOff)

      meanSquaredErrorOfTotalEnergy(//[Vector2D, Context, ListList, PeriodicLennardJonesFrameLog](
        buildPeriodicLJFrameLog(particlesState, box),
        numberOfFrames,
      )
    }
  }

  def energyErrorOfCells3DSimulation[Context[_] : Monad](
                                                         density: Double,
                                                         particlesSideNumber: Int,
                                                         velocityFactor: Double,
                                                         numberOfFrames: Int,
                                                         `∆t`: Double = 0.0001,
                                                         rCutOff: Double = 5.0)(implicit par : Parallel[Context]): Option[Context[Double]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector3D, CubicFigure] = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val particlesState = PeriodicParticlesCells3D.create[Context](
        makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor),
        box,
        rCutOff
      )

      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = LennardJonesPotential.periodicCutOffPotential(box)(rCutOff)

      meanSquaredErrorOfTotalEnergy(
        buildPeriodicLJFrameLog(particlesState, box),
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
        2000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 100*100=10000 molecules are presented with density=0.65" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.65,
        100,
        1.24,
        2000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
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
        1000,
        0.001
      )

      energyError must beSome((err: Double) => err < 1E-5)
    }

    "Save total energy when 1000 molecules are presented with density=0.8" >> {
      val energyError: Option[Double] = energyErrorOfCells2DSimulation[Id](
        0.8,
        10,
        1.24,
        1000,
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
