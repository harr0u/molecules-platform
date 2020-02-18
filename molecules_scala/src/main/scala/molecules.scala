import java.io.{BufferedWriter, File, FileWriter}

import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.limitConditions.SpaceConditions
import domain.Particle
import domain.geometry.vector._
import calculation.numerical.LeapFrogIteration
import calculation.physics.{CenterOfMassCalculator, LennardJonesCutOffPotential, LennardJonesPotential}
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import state.cells.{PeriodicFutureParticlesCells2D, PeriodicFutureParticlesCells3D}
import state.{ParticlesSeqState, ParticlesState, ParticlesStateReducer}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random


object Molecules extends App {
  val numberOfFrames = 2500

  val framesHistory: LazyList[Future[(Double, Long)]] = buildTotalEnergyLog(
    boxWidth = 60.0,
    sideNumber = 50,
    velocityFactor = 1.5,
    smart = true
  )

  val futureTotalEnergyCheck: Future[Unit] = for {
    (initialTotal, initialEpoch) <- framesHistory(0)
    (lastTotal, lastEpoch) <- framesHistory(numberOfFrames - 1)
  } yield {
    println(f"Total change = ${lastTotal - initialTotal}; FPS = ${numberOfFrames * 1000.0 / (lastEpoch - initialEpoch)}")
  }

  Await.result(futureTotalEnergyCheck, Duration.Inf)


  def buildTotalEnergyLog(boxWidth: Double, sideNumber: Int, velocityFactor: Double, smart: Boolean): LazyList[Future[(Double, Long)]] = {
    val (numberOfParticles: Int, moleculesLog) = build2DLog(boxWidth, sideNumber, velocityFactor, smart)

    moleculesLog.zipWithIndex.map {
      case (iter, i) => iter.map((iteration) => {
        val kineticEnergy: Double = iteration.particles.counit.map((p) => p.velocity.squaredLength / 2).iterator.sum / numberOfParticles
        val potential: Double = iteration.particles.counit.map(_.potential).iterator.sum / numberOfParticles
        val total: Double = kineticEnergy + potential

        if (i % 25 == 0) {
          println(f"Frame ${i} - T[${total}] - P[${potential}] - K[${kineticEnergy}] - FPS")
        }

        (total, System.currentTimeMillis)
      })
    }
  }

  def build3DLog(boxWidth: Double, sideNumber: Int, velocityFactor: Double, smart: Boolean = true): (Int, LazyList[Future[LeapFrogIteration[Vector3D, CubicFigure]]]) = {
    val box: BoxPeriodicSpaceConditions = new BoxPeriodicSpaceConditions(Cube(boxWidth));
    val superSmartMolecules = (smart: Boolean) => {
      val rng = new Random(0L)

      val particles: Seq[Particle[Vector3D]] = for {
        i <- 0 until sideNumber
        j <- 0 until sideNumber
        k <- 0 until sideNumber
      } yield {
        new Particle[Vector3D](
          i * sideNumber * sideNumber + j * sideNumber + k,
          Vector3D((i + 0.5) * boxWidth / sideNumber, (j + 0.5) * boxWidth / sideNumber, (k + 0.5) * boxWidth / sideNumber),
          Vector3D(rng.nextDouble(), rng.nextDouble(), rng.nextDouble()) * velocityFactor,
          Vector3D.empty,
          potential = 0.0,
          mass = 1.0
        )
      }

      if (smart) PeriodicFutureParticlesCells3D.create(particles, box) else ParticlesSeqState(particles)
    }

    buildLog(superSmartMolecules(smart), box)
  }

  def build2DLog(boxWidth: Double, sideNumber: Int, velocityFactor: Double, smart: Boolean = true): (Int, LazyList[Future[LeapFrogIteration[Vector2D, RectangleFigure]]]) = {
    val box: RectanglePeriodicSpaceConditions = new RectanglePeriodicSpaceConditions(Square(boxWidth));

    val superSmartMolecules = (smart: Boolean) => {
      val rng = new Random(0L)

      val particles: Seq[Particle[Vector2D]] = for {
        i <- 0 until sideNumber
        j <- 0 until sideNumber
      } yield {
        new Particle[Vector2D](
          i * sideNumber + j,
          Vector2D((i + 0.5) * boxWidth / sideNumber, (j + 0.5) * boxWidth / sideNumber),
          Vector2D(rng.nextDouble(), rng.nextDouble()) * velocityFactor,
          Vector2D.empty,
          potential = 0.0,
          mass = 1.0
        )
      }

      if (smart) {
        PeriodicFutureParticlesCells2D.create(particles, box, minimumCellLength = 5.0)
      } else {
        ParticlesSeqState(particles)
      }
    }


    buildLog(superSmartMolecules(smart), box)
  }

  def buildLog[V <: AlgebraicVector[V], Fig <: GeometricFigure](
    particles : ParticlesState[V, Future],
    box: SpaceConditions[V, Fig]
  ): (Int, LazyList[Future[LeapFrogIteration[V, Fig]]]) = {
    val numberOfParticles = particles.counit.length

    val makeIteration: () => Future[LeapFrogIteration[V, Fig]] = () => new LeapFrogIteration(
      particles,
      new ParticlesStateReducer(),
      new LennardJonesCutOffPotential(cutOffRadius = 5.0),
      box,
      `∆t` = 0.001
    ).init()


    val particlesLog = LazyList.iterate(makeIteration())(
      iter => iter.flatMap(iteration => iteration.iterationStep())
    )

    (numberOfParticles, particlesLog)
  }
}