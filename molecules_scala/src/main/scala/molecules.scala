import java.io.{BufferedWriter, File, FileWriter}

import calculation.geometry.figures.{Cube, CubicFigure}
import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.geometry.vector._
import molecules.ParticlesSimpleState
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesPotential
import state.ParticlesCells3D


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

    new ParticlesCells3D(particles, box)
  }

  val makeIteration: () => LeapFrogIteration[Vector3D, CubicFigure] = () => new LeapFrogIteration(
    superSmartMolecules(),
    new LennardJonesPotential[Vector3D](),
    box,
    deltaStepTime = 0.001
  ).init()


  val moleculesLog = LazyList.iterate(makeIteration())(
    iter => iter.iterationStep()
  )

  val totals = moleculesLog.map((iter: LeapFrogIteration[Vector3D, CubicFigure]) => {
    val kineticEnergy: Double = iter.particles.particlesStream.map((p) => Math.pow(p.velocity.length, 2) / 2).iterator.sum
    val potential: Double = iter.particles.particlesStream.map(_.potential).iterator.sum
    val total: Double = kineticEnergy + potential

    (total, System.currentTimeMillis)
  })

  val numberOfFrames = 1000
  val (initTotal, initEpoch) = totals.head
  val (lastTotal, epoch) = totals(numberOfFrames)

  println(f"Total change = ${lastTotal - initTotal}; FPS = ${numberOfFrames * 1000.0 / (epoch - initEpoch)}")
}