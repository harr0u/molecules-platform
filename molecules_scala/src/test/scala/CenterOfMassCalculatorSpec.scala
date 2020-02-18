import org.scalatest._
import calculation.physics.CenterOfMassCalculator._
import domain.Particle
import domain.geometry.vector._
import state.ParticlesSeqState

class CenterOfMassCalculatorSpec extends FlatSpec with Matchers {
  val empty2DParticle = Particle(0, Vector2D.empty, Vector2D.empty, Vector2D.empty, 0.0, 1.0)
  val empty3DParticle = Particle(0, Vector3D.empty, Vector3D.empty, Vector3D.empty, 0.0, 1.0)

  val modal2DVector = Vector2D(10, 20)
  val modal3DVector = Vector3D(69, 88, 14)

  "findCenterMassVelocity" should "return None for empty ParticlesState" in {
    val state2D = new ParticlesSeqState[Vector2D](Seq.empty[Particle[Vector2D]])
    val state3D = new ParticlesSeqState[Vector3D](Seq.empty[Particle[Vector3D]])

    assert(findCenterMassVelocity(state2D).isEmpty)
    assert(findCenterMassVelocity(state3D).isEmpty)
  }

  it should "return Velocity of particle if it is lonely particle in system" in {
    val state2D = new ParticlesSeqState[Vector2D](Seq(empty2DParticle.copy(velocity = modal2DVector)))
    val state3D = new ParticlesSeqState[Vector3D](Seq(empty3DParticle.copy(velocity = modal3DVector)))

    assert(findCenterMassVelocity(state2D).contains(modal2DVector))
    assert(findCenterMassVelocity(state3D).contains(modal3DVector))
  }

  it should "return zero Vector if system contains 2 identical particles with opposite velocities" in {
    val state2D = new ParticlesSeqState[Vector2D](Seq(
      empty2DParticle.copy(velocity = modal2DVector),
      empty2DParticle.copy(velocity = modal2DVector * -1),
    ))
    val state3D = new ParticlesSeqState[Vector3D](Seq(
      empty3DParticle.copy(velocity = modal3DVector),
      empty3DParticle.copy(velocity = modal3DVector * -1),
    ))

    assert(findCenterMassVelocity(state2D).contains(Vector2D.empty))
    assert(findCenterMassVelocity(state3D).contains(Vector3D.empty))
  }

  "findCenterMassPosition" should "return None for empty ParticlesState" in {
    val state2D = new ParticlesSeqState[Vector2D](Seq.empty[Particle[Vector2D]])
    val state3D = new ParticlesSeqState[Vector3D](Seq.empty[Particle[Vector3D]])

    assert(findCenterMassPosition(state2D).isEmpty)
    assert(findCenterMassPosition(state3D).isEmpty)
  }

  it should "return position of particle in case of alone Particle" in {
    val state2D = new ParticlesSeqState[Vector2D](Seq(empty2DParticle.copy(position = modal2DVector)))
    val state3D = new ParticlesSeqState[Vector3D](Seq(empty3DParticle.copy(position = modal3DVector)))

    assert(findCenterMassPosition(state2D).contains(modal2DVector))
    assert(findCenterMassPosition(state3D).contains(modal3DVector))
  }
}
