package e2e

import calculation.physics.Utils
import calculation.space.SpaceConditions
import calculation.space.periodic.RectanglePeriodicSpaceConditions
import cats.{Id, Monad, Parallel}
import cats.implicits._
import domain.geometry.figures.{RectangleFigure, Square}
import domain.geometry.vector.Vector2D
import e2e.FrameLogTester._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FutureMatchers
import simulation.frameLog.{FrameLog, IdealGasFrameLog, IdealGasFrameLog2D}
import simulation.state.ParticlesListState
import org.specs2.mutable.Specification

class IdealGasTests(implicit ee: ExecutionEnv) extends Specification with FutureMatchers {
  def idealGasFrameLog2D[Context[_]](
    density: Double,
    particlesSideNumber: Int,
    velocityFactor: Double,
    numberOfFrames: Int,
    `∆t`: Double = 0.0001
)(implicit P: Parallel[Context], Context : Monad[Context]): Option[Context[List[IdealGasFrameLog[Vector2D, Context]]]] = {
    for {
      boxWidth <- Utils.calculateWidthWithSideCount(particlesSideNumber, density)
      if velocityFactor > 0.0 && numberOfFrames > 0
    } yield {
      val box: SpaceConditions[Vector2D, RectangleFigure] = RectanglePeriodicSpaceConditions(Square(boxWidth))
      val particlesState = ParticlesListState[Vector2D, Context](makeParticlesIn(box.boundaries, particlesSideNumber, velocityFactor))

      Context.flatMap(IdealGasFrameLog.create[Context](particlesState, box).get.init)(fl => List.fill(numberOfFrames)(fl.next).sequence)
    }
  }

  "2D" >> {
    "random position when 1 molecule is presented" >> {
      idealGasFrameLog2D[Id](0.05,1,1.2,100)
        .map(_.map(_.particles.getParticles(0).position.length).iterator.sum / 100) must beSome((x: Double) => x > 7.0)
    }
  }
}
