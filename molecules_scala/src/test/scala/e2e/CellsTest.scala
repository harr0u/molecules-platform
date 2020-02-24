package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesCutOffPotential, LennardJonesPeriodicCutOffPotential, LennardJonesPeriodicPotential, LennardJonesPotential, PotentialCalculator}
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import e2e.FrameLogTester._
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
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 100.0
      val rCutOff = 5.0
      val velocityFactor = 50.0
      val box = RectanglePeriodicSpaceConditions(Square(boxWidth))
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicCutOffPotential[Vector2D, RectangleFigure](box, rCutOff)

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, 1, velocityFactor), box),
        box,
        10E1.toInt,
        1,
      )

      energyError must be_<(10E-10).await(retries = 1, timeout = 120.seconds)
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
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicCutOffPotential[Vector2D, RectangleFigure](box, rCutOff)
      val particles: PeriodicFutureParticlesCells2D = {
        val velocityFactor = 1.2
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        5E3.toInt,
        225,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)
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
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicCutOffPotential[Vector2D, RectangleFigure](box, rCutOff)
      val particles = {
        val velocityFactor = 1.3
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        5E3.toInt,
        625,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)
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
      implicit val potentialCalculator: PotentialCalculator[Vector2D] = new LennardJonesPeriodicCutOffPotential[Vector2D, RectangleFigure](box, rCutOff)
      val particles = {
        val velocityFactor = 1.3
        PeriodicFutureParticlesCells2D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector2D, RectangleFigure](
        particles,
        box,
        100.toInt,
        10000,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)
    }
  }


  "3D" >> {
    "Save total energy when 1 molecule is presented" >> {
      val boxWidth = 20.0
      val velocityFactor = 50.0
      val box = BoxPeriodicSpaceConditions(Cube(boxWidth))
      val rCutOff = 5.0

      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicCutOffPotential[Vector3D, CubicFigure](box, rCutOff)


      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, 1, velocityFactor), box),
        box,
        1E4.toInt,
        1,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-5).await(1, 30.seconds)
    }

    "Save total energy when 27 molecules are presented with density=0.7" >> {
      val density = 0.7
      val particlesSideNumber = 3
      val rCutOff = 5.0

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicCutOffPotential[Vector3D, CubicFigure](box, rCutOff)
      val particles: PeriodicFutureParticlesCells3D = {
        val velocityFactor = 12.0
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }

      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        particles,
        box,
        1E3.toInt,
        27,
        `∆t` = 0.0005
      )

      energyError must be_<(1E-2).await(retries = 1, timeout = 120.seconds)

    }

    "Save total energy when 1000 molecules are presented with density=0.8" >> {
      val density = 0.8
      val particlesSideNumber = 10
      val rCutOff = 5.0

      val box: BoxPeriodicSpaceConditions = {
        val boxWidth = particlesSideNumber / density
        BoxPeriodicSpaceConditions(Cube(boxWidth))
      }
      implicit val potentialCalculator: PotentialCalculator[Vector3D] = new LennardJonesPeriodicCutOffPotential[Vector3D, CubicFigure](box, rCutOff)
      val particles: PeriodicFutureParticlesCells3D = {
        val velocityFactor = 1.2
        PeriodicFutureParticlesCells3D.create(makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor), box, rCutOff)
      }


      val energyError: Future[Double] = meanSquaredErrorOfTotalEnergy[Vector3D, CubicFigure](
        particles,
        box,
        50,
        1000,
        `∆t` = 0.001
      )

      energyError must be_<(1E-4).await(retries = 1, timeout = 120.seconds)
    }
  }
}
