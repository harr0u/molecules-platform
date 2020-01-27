import org.scalatest._
import domain.Particle
import domain.vector._

class ParticleSpec extends FlatSpec with Matchers {
  "A Particle" should "change its position after translating" in {
    val particle = new Particle(Vector3D(1, 2, 3), Vector3D.empty)

    particle.position = particle.position + Vector3D(10, 10, 10)

    assert(particle.position.x == 11)
    assert(particle.position.y == 12)
    assert(particle.position.z == 13)
  }

  it should "store unique immutable id" in {
    val p1 = new Particle(Vector3D.empty, Vector3D.empty)
    val p2 = new Particle(Vector3D.empty, Vector3D.empty)

    assert(p1.id != p2.id)
  }
}