import domain.Particle
import domain.vector._
import molecules.ParticlesSimpleSet
import calculation.numerical.LeapFrogIteration
import calculation.physics.PeriodicLennardJonesPotential

object Molecules extends App {
  val particleX: Particle[Vector2D] = new Particle[Vector2D](Vector2D(1, 8), Vector2D.empty)
  val particleY: Particle[Vector2D] = new Particle[Vector2D](Vector2D(2, -3), Vector2D.empty)

  val molecules: ParticlesSimpleSet[Vector2D] = new ParticlesSimpleSet(List(particleX, particleY));
  molecules.limitConditions()

  val boxWidth: Double = 100.0

  val potentialCalculator = new PeriodicLennardJonesPotential[Vector2D](boxWidth)
  println(potentialCalculator.computeForceAndPotential(particleX.position, particleY.position))

  val iterator: LeapFrogIteration[Vector2D] = new LeapFrogIteration(molecules, potentialCalculator)
  iterator.init()
  iterator.iterationStep()
}