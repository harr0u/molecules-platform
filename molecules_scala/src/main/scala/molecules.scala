import java.io.{BufferedWriter, File, FileWriter}

import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.geometry.vector._
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesPotential
import domain.geometry.figures.{Cube, CubicFigure}
import state.{ParticlesPhysicsReducer, ParticlesSeqState, PeriodicParticlesCells3D}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object Molecules extends App {
  val boxWidth: Double = 50.0

  val box: BoxLimitConditions = new BoxLimitConditions(Cube(boxWidth));

  val superSmartMolecules = (smart: Boolean) => {
    val velocityFactor = 0.12;
    val sideNumber = 25;
    val particles: Seq[Particle[Vector3D]] = for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
      k <- 0 until sideNumber
    } yield {
      new Particle[Vector3D](
        i * sideNumber * sideNumber + j * sideNumber + k,
        Vector3D((i + 0.5) * boxWidth / sideNumber, (j + 0.5) * boxWidth / sideNumber, (k + 0.5) * boxWidth / sideNumber),
        Vector3D(Math.random(), Math.random(), Math.random()) * velocityFactor,
        Vector3D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }

    if (smart) PeriodicParticlesCells3D.create(particles, box, 3.0) else ParticlesSeqState(particles)
  }

  val makeIteration: () => Future[LeapFrogIteration[Vector3D, CubicFigure]] = () => new LeapFrogIteration(
    superSmartMolecules(true),
    new ParticlesPhysicsReducer(),
    new LennardJonesPotential(),
    box,
    deltaStepTime = 0.001
  ).init()


  val moleculesLog = LazyList.iterate(makeIteration())(
    iter => iter.flatMap(iteration => iteration.iterationStep())
  )

  val framesHistory = moleculesLog.map((iter: Future[LeapFrogIteration[Vector3D, CubicFigure]]) => {
    iter.map((iteration) => {
      val kineticEnergy: Double = iteration.particles.counit.map((p) => Math.pow(p.velocity.length, 2) / 2).iterator.sum
      val potential: Double = iteration.particles.counit.map(_.potential).iterator.sum
      val total: Double = kineticEnergy + potential
      (total, System.currentTimeMillis)
    })
  })

  val numberOfFrames = 20
  val firstFrame = framesHistory(0)
  val lastFrame = framesHistory(numberOfFrames - 1)


  val kek = for {
    (initialTotal, initialEpoch) <- firstFrame
    (lastTotal, lastEpoch) <- lastFrame
  } yield {
    println(f"Total change = ${lastTotal - initialTotal}; FPS = ${numberOfFrames * 1000.0 / (lastEpoch - initialEpoch)}")
  }

  Await.result(kek, Duration.Inf)
}