import calculation.geometry.figures.{Cube, CubicFigure}
import calculation.limit_conditions.BoxLimitConditions
import domain.Particle
import domain.vector._
import molecules.ParticlesSimpleSet
import calculation.numerical.LeapFrogIteration
import calculation.physics.LennardJonesPotential

object Molecules extends App {
  val particleX: Particle[Vector3D] = new Particle[Vector3D](Vector3D(1, 8), Vector3D.empty)
  val particleY: Particle[Vector3D] = new Particle[Vector3D](Vector3D(2, -3), Vector3D.empty)

  val boxWidth: Double = 100.0

  val iterator: LeapFrogIteration[Vector3D, Cube] = new LeapFrogIteration(
    new ParticlesSimpleSet[Vector3D](List(particleX, particleY)),
    new LennardJonesPotential[Vector3D](),
    new BoxLimitConditions(new Cube(boxWidth))
  )


  iterator.init()
  iterator.iterationStep()
}