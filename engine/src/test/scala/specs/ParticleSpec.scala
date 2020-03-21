package specs

import domain.Particle
import domain.geometry.vector._
import org.scalatest._

class ParticleSpec extends FlatSpec with Matchers {
  "A Particle" should "contain acceleration which depends on Force (F = ma)" in {
    val force: Vector3D = Vector3D(100, 200, 300)
    val mass: Double = 100.0

    val particle = new Particle(0, Vector3D.empty, Vector3D.empty, force, 0.0, mass)

    assert(particle.acceleration == Vector3D(1, 2, 3))
  }
}