import java.io.{BufferedWriter, File, FileWriter}

import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.geometry.vector._
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesCutOffPotential, LennardJonesPotential}
import domain.geometry.figures.{Cube, CubicFigure}
import state.cells.PeriodicParticlesCells3D
import state.{ParticlesPhysicsReducer, ParticlesSeqState}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random


object Molecules extends App {
  val boxWidth: Double = 15.0

  val box: BoxLimitConditions = new BoxLimitConditions(Cube(boxWidth));

  val superSmartMolecules = (smart: Boolean) => {
    val rng = new Random(0L)

    val velocityFactor = 0.5;
    val sideNumber = 15;
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

    if (smart) PeriodicParticlesCells3D.create(particles, box, 5.0) else ParticlesSeqState(particles)
  }

  val particles = superSmartMolecules(true)
  val numberOfParticles = particles.counit.length

  val makeIteration: () => Future[LeapFrogIteration[Vector3D, CubicFigure]] = () => new LeapFrogIteration(
    particles,
    new ParticlesPhysicsReducer(),
    new LennardJonesCutOffPotential(),
    box,
    deltaStepTime = 0.001
  ).init()


  val moleculesLog = LazyList.iterate(makeIteration())(
    iter => iter.flatMap(iteration => iteration.iterationStep())
  )

  val framesHistory: LazyList[Future[(Double, Long)]] = moleculesLog.map((iter: Future[LeapFrogIteration[Vector3D, CubicFigure]]) => {
    iter.map((iteration) => {
      val kineticEnergy: Double = iteration.particles.counit.map((p) => Math.pow(p.velocity.length, 2) / 2).iterator.sum / numberOfParticles
      val potential: Double = iteration.particles.counit.map(_.potential).iterator.sum / numberOfParticles
      val total: Double = kineticEnergy + potential
      (total, System.currentTimeMillis)
    })
  })

  val numberOfFrames = 20

  Await.result(
    for {
      (initialTotal, initialEpoch) <- framesHistory(0)
      (lastTotal, lastEpoch) <- framesHistory(numberOfFrames - 1)
    } yield {
      println(f"Total change = ${lastTotal - initialTotal}; FPS = ${numberOfFrames * 1000.0 / (lastEpoch - initialEpoch)}")
    },
    Duration.Inf
  )
}