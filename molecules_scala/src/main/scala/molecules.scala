import java.io.{BufferedWriter, File, FileWriter}

import calculation.geometry.figures.{Cube, CubicFigure}
import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.vector._
import molecules.ParticlesSimpleSet
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesPotential
import molecule_containers.ParticlesCells3D


object Molecules extends App {
  val boxWidth: Double = 13.0

  val particleX: Particle[Vector3D] = new Particle[Vector3D](Vector3D(1, -5), Vector3D(0.01))
  val particleY: Particle[Vector3D] = new Particle[Vector3D](Vector3D(2, -3), Vector3D(0.0, 0.0, 0.01))
  val superDummyMolecules = () => new ParticlesSimpleSet[Vector3D](List(particleX, particleY))

  val box: BoxLimitConditions = new BoxLimitConditions(Cube(boxWidth));

  val superSmartMolecules = () => {
    val velocityFactor = 0.12;
    val sideNumber = 5;
    val particles: Seq[Particle[Vector3D]] = for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
      k <- 0 until sideNumber
    } yield {
      new Particle(
        Vector3D((i + 0.5) * boxWidth / sideNumber, (j + 0.5) * boxWidth / sideNumber, (k + 0.5) * boxWidth / sideNumber),
        Vector3D(Math.random(), Math.random(), Math.random()) * velocityFactor
      )
    }
    new ParticlesSimpleSet(particles)
  }

  val iterator: LeapFrogIteration[Vector3D, CubicFigure] = new LeapFrogIteration(
    superSmartMolecules(),
    new LennardJonesPotential[Vector3D](),
    box,
    0.001
  )
  iterator.init()

  // log is verb
  val logMolecules: () => Seq[Particle[Vector3D]] = () => iterator.particles.particlesStream
  // log is noun
  val moleculesLog: LazyList[Seq[Particle[Vector3D]]] = LazyList.iterate(logMolecules())((__) => {
    iterator.iterationStep()
    logMolecules()
  })

  val totals = moleculesLog.map((particles: Seq[Particle[Vector3D]]) => {
    val kineticEnergy: Double = particles.foldLeft(0.0)((acc, p) => Math.pow(p.velocity.length, 2) / 2 + acc)
    val potential: Double = particles.map(_.potential).iterator.sum
    val total: Double = kineticEnergy + potential

    (total, System.currentTimeMillis)
  })

  println(totals.take(1)(0))

  println(totals.take(Math.pow(10, 3).toInt)(Math.pow(10, 3).toInt - 1))
  // FileWriter
}