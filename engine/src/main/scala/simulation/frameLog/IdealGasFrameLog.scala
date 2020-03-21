package simulation.frameLog
import breeze.stats.distributions.{Rand, Uniform}
import calculation.space.SpaceConditions
import domain.Particle
import cats.{Id, Monad, Parallel}
import cats.implicits._
import domain.geometry.figures.RectangleFigure
import domain.geometry.vector.{AlgebraicVector, Vector2D}
import simulation.actions.ParticlesChangeAction
import simulation.state.ParticlesListState
import molecules.Utils._
import simulation.reducers.ParticlesStateReducer
import simulation.{ParticlesReducer, ParticlesState, state}

case class IdealGasFrameLog2D[Context[_]](
                                                                    override val particles: ParticlesState[Vector2D, Context, List],
                                                                    spaceConditions: SpaceConditions[Vector2D, RectangleFigure],
                                                                    override val particlesReducer: ParticlesReducer[Vector2D, Context, List],
                                                                    override val meanVelocity: Double,
                                                                    override val count: Int
                                                                  )(implicit Parallel: Parallel[Context], Context: Monad[Context]) extends IdealGasFrameLog[Vector2D, Context] {

  override protected def updateWithParticlesState(particles: ParticlesState[Vector2D, Context, List]): Context[IdealGasFrameLog[Vector2D, Context]] = {
    Context.pure(this.copy(particles = particles))
  }

  protected def makeParticlesDistribution(meanVelocity: Double): Rand[Particle[Vector2D]] = {
    val speedDistr = Uniform(0.0, meanVelocity * 2)
    val angleDistr = Uniform(0.0, 2 * Math.PI)
    val positionXDistr = Uniform(0.0, spaceConditions.boundaries.width)
    val positionYDistr = Uniform(0.0, spaceConditions.boundaries.width)

    var id = -1

    for {
      speed <- speedDistr
      angle <- angleDistr
      x <- positionXDistr
      y <- positionYDistr
    } yield {
      id += 1

      Particle(
        id = id,
        position = Vector2D(x, y),
        velocity = Vector2D(speed * Math.cos(angle), speed * Math.sin(angle)),
        force = Vector2D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }
  }
}

abstract class IdealGasFrameLog[V <: AlgebraicVector[V], Context[_] : Monad](implicit Parallel : Parallel[Context]) extends FrameLog[V, Context, List, IdealGasFrameLog[V, Context]] {
  protected def makeParticlesDistribution(meanVelocity: Double): Rand[Particle[V]]

  def meanVelocity: Double
  def count: Int

  override def init: Context[IdealGasFrameLog[V, Context]] = updateWithParticlesState(regenerateParticle)
  override def next: Context[IdealGasFrameLog[V, Context]] = updateWithParticlesState(regenerateParticle)

  protected def regenerateParticle: ParticlesState[V, Context, List] = {
    makeParticlesDistribution(meanVelocity).sample(count).toList |> ParticlesListState[V, Context]
  }
}

object IdealGasFrameLog {
  def create[Context[_]](
                                  particles: ParticlesState[Vector2D, Context, List],
                                  spaceConditions: SpaceConditions[Vector2D, RectangleFigure],
                                )(implicit parallel: Parallel[Context], Context : Monad[Context]): Option[IdealGasFrameLog[Vector2D, Context]] = {
    particles
      .getParticles
      .length.zeroToNone
      .map(count =>
        IdealGasFrameLog2D(
          particles,
          spaceConditions,
          new ParticlesStateReducer[Vector2D, Context, List],
          particles.getParticles.map(_.velocity.squaredLength).sum / count,
          count
        )
      )
  }
}