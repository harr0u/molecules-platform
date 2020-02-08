import java.io.{BufferedWriter, File, FileWriter}

import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.geometry.vector._
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesPotential
import domain.geometry.figures.{Cube, CubicFigure}
import state.{ParticlesPhysicsReducer, ParticlesSeqState, PeriodicParticlesCells3D}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object Molecules extends App {
  val boxWidth: Double = 13.0

  val box: BoxLimitConditions = new BoxLimitConditions(Cube(boxWidth));

  val superSmartMolecules = () => {
    val velocityFactor = 0.12;
    val sideNumber = 5;
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

    new ParticlesSeqState(particles)
  }

  val makeIteration: () => Future[LeapFrogIteration[Vector3D, CubicFigure]] = () => new LeapFrogIteration(
    superSmartMolecules(),
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

  val numberOfFrames = 10000

  for {
    (initialTotal, initialEpoch) <- framesHistory(0)
    (lastTotal, lastEpoch) <- framesHistory(numberOfFrames - 1)
  } yield {
    println(f"Total change = ${lastTotal - initialTotal}; FPS = ${numberOfFrames * 1000.0 / (lastEpoch - initialEpoch)}")
  }
}