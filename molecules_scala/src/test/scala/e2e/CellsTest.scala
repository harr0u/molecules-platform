package e2e

import calculation.physics.potentials.{CutOffPotentialOptimization, LennardJonesPotential, PeriodicPotential}
import calculation.space.SpaceConditions
import calculation.space.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import cats.Id
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import e2e.FrameLogTester._
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import simulation.ParticlesState
import state.state.cells.{PeriodicParticlesCells2D, PeriodicParticlesCells3D}

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations
import cats.implicits._

class CellsTest(implicit ee: ExecutionEnv) extends mutable.Specification with FutureMatchers with FrameLogTester {
  sequential

  private def makePotential[V <: AlgebraicVector[V], Fig <: GeometricFigure](box: SpaceConditions[V, Fig], rCutOff: Double): LennardJonesPotential[V] = {
    new LennardJonesPotential[V]
      with PeriodicPotential[V, Fig]
      with  CutOffPotentialOptimization[V]
    {
      override def limitConditions: SpaceConditions[V, Fig] = box
      override def cutOffDistance: Double = rCutOff
    }
  }

  "2D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val rCutOff = 5.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box, rCutOff)

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(
          PeriodicParticlesCells2D.create[Id](makeParticlesIn(box.boundaries, 1, velocityFactor), box),
          box
        ),
        100,
      )

      energyError must be_<(10E-10)
    }

    "Save total energy when 15*15=225 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 15

      val box: RectanglePeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        println(s"boxWidth ~> $boxWidth")
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }

      val rCutOff = 5.0
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box, rCutOff)
      val particles: PeriodicParticlesCells2D[Id] = {
        val velocityFactor = 1.2
        PeriodicParticlesCells2D.create[Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000,
      )

      energyError must be_<(1E-2)
    }

    "Save total energy when 25*25=625 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 25

      val rCutOff = 5.0

      val box = {
        val boxWidth = particlesSideNumber / density
        println(boxWidth)
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box, rCutOff)
      val particles = {
        val velocityFactor = 1.3
        PeriodicParticlesCells2D.create[Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        5000
      )

      energyError must be_<(1E-2)
    }

    "Save total energy in 100F when 100*100=10E4 molecules are presented with density=0.6" >> {
      val density = 0.6
      val particlesSideNumber = 100
      val rCutOff = 5.0

      val box = {
        val boxWidth = particlesSideNumber / density
        println(boxWidth)
        RectanglePeriodicSpaceConditions(Square(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector2D] = makePotential(box, rCutOff)
      val particles = {
        val velocityFactor = 1.3
        PeriodicParticlesCells2D.create[Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        100,
      )

      energyError must be_<(1E-2)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 20.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val rCutOff = 5.0

      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box, rCutOff)

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure, Id](
        buildFrameLog(
          PeriodicParticlesCells3D.create[Id](makeParticlesIn(box.boundaries, 1, velocityFactor), box),
          box,
          `∆t` = 0.0005
        ),
        500,
      )

      energyError must be_<(1E-5)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3
      val rCutOff = 5.0

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box, rCutOff)
      val particles: PeriodicParticlesCells3D[Id] = {
        val velocityFactor = 12.0
        PeriodicParticlesCells3D.create[Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.0005),
        1000,
      )

      energyError must be_<(1E-2)

    }

    "Save total energy when 1000 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10
      val rCutOff = 5.0

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: LennardJonesPotential[Vector3D] = makePotential(box, rCutOff)
      val particles: PeriodicParticlesCells3D[Id] = {
        val velocityFactor = 1.2
        PeriodicParticlesCells3D.create[Id](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }


      val energyError: Id[Double] = meanSquaredErrorOfTotalEnergy(
        buildFrameLog(particles, box, `∆t` = 0.001),
        40,
      )

      energyError must be_<(1E-4)
    }
  }
}
