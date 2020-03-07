package specs

import domain.Particle
import domain.geometry.vector.{AlgebraicVector, Vector2D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.fp.Id
import simulation.ParticlesState
import simulation.actions.UpdatePositions
import simulation.reducers.ParticlesStateReducer
import cats.implicits._
import org.specs2._

class ParticlesStateReducerSpec(implicit ee: ExecutionEnv) extends mutable.Specification {
  "Should apply Actions consistently" >> {
    "position.x: 10 |> (+1) |> (*2) = 22" >> {
      val reducer = new ParticlesStateReducer[Vector2D, Id, Id]()
      val state: ParticlesState[Vector2D, Id, Id] = SingleParticleState[Vector2D](Particle(0, Vector2D(10, 12), Vector2D.empty, Vector2D.empty, 0.0, 1.0))

      val newState = reducer.applyChangeActions(state)(List(
        UpdatePositions(p => p.position + Vector2D(1, 1)),
        UpdatePositions(p => p.position * 2),
      ))

      newState.getParticles.length must_=== 1
      newState.getParticles(0).position.x must_=== 22
      newState.getParticles(0).position.y must_=== 26
    }

    "force.x: -5 |> (abs) |> (-5) = 0" >> {
      val reducer = new ParticlesStateReducer[Vector2D, Id, Id]()
      val state: ParticlesState[Vector2D, Id, Id] = SingleParticleState[Vector2D](Particle(0, Vector2D(-5, -5), Vector2D.empty, Vector2D.empty, 0.0, 1.0))

      val newState = reducer.applyChangeActions(state)(List(
       UpdatePositions(p => p.position.mapCoordinates(Math.abs)),
       UpdatePositions(p => p.position - Vector2D(5, 5)),
      ))

      newState.getParticles.length must_=== 1
      newState.getParticles(0).position.x must_=== 0
    }
  }
}

case class SingleParticleState[V <: AlgebraicVector[V]](particle: Particle[V]) extends ParticlesState[V, Id, Id] {
  override def getParticles: Seq[Particle[V]] = Seq(particle)

  override def counit: Particle[V] = particle

  override def updateWithParticles(particles: Id[Particle[V]]): Id[ParticlesState[V, Id, Id]] = this.copy[V](particle = particles)
}