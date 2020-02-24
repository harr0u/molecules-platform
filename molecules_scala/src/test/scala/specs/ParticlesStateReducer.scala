package specs

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.LeapFrogIteration
import calculation.physics.{LennardJonesPotential, PotentialCalculator}
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.fp.Id
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import state.{ParticlesSeqState, ParticlesState, ParticlesStateReducer}
import state.cells.{PeriodicFutureParticlesCells2D, PeriodicFutureParticlesCells3D}

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations

class ParticlesStateReducerSpec(implicit ee: ExecutionEnv) extends mutable.Specification {
  "Should apply Actions consistently" >> {
    "position.x: 10 |> (+1) |> (*2) = 22" >> {
      val reducer = new ParticlesStateReducer[Vector2D, Id]()
      val state = SingleParticleState[Vector2D](Particle(0, Vector2D(10, 12), Vector2D.empty, Vector2D.empty, 0.0, 1.0))

//      reducer.applyChangeActions(state, List(
//
//      ))
      1 must_=== 1
    }
  }
}

case class SingleParticleState[V <: AlgebraicVector[V]](p: Particle[V]) extends ParticlesState[V, Id] {
  override def counit: LazyList[Particle[V]] = LazyList(p)
  override def map(fn: Particle[V] => Particle[V]): ParticlesState[V, Id] = SingleParticleState(fn(p))

  override def reduce(fn: (Particle[V], Particle[V]) => Particle[V]): ParticlesState[V, Id] = this
  override def unit(): ParticlesState[V, Id] = this
}