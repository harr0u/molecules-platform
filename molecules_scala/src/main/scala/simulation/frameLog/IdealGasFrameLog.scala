package simulation.frameLog
import cats.{Id, Monad}
import domain.geometry.vector.AlgebraicVector
import simulation.actions.ParticlesChangeAction
import simulation.state.ParticlesListState
import simulation.{ParticlesReducer, ParticlesState}

case class IdealGasFrameLog[V <: AlgebraicVector[V], Context[_] : Monad](
                                                                    override val particles: ParticlesState[V, Context, List],
                                                                    override val particlesReducer: ParticlesReducer[V, Context, List],
                                                                  ) extends FrameLog[V, Context, List] {

  override protected def initActions: Context[Seq[ParticlesChangeAction[V]]] = makeNewParticlesAnsamble

  override def nextActions: Context[Seq[ParticlesChangeAction[V]]] = initActions

  override protected def updateWithParticles(particles: ParticlesState[V, Context, List]): FrameLog[V, Context, List] = {
    this.copy(particles = particles)
  }

  protected def makeNewParticlesAnsamble: Context[Seq[ParticlesChangeAction[V]]] = {
    
    Context.pure(Seq.concat(
      Seq(), // positions
      Seq(), // velocities
    ))
  }
}
