package simulation.frameLog
import cats.{Id, Monad}
import domain.geometry.vector.AlgebraicVector
import simulation.actions.ParticlesChangeAction
import simulation.state.ParticlesListState
import simulation.{ParticlesReducer, ParticlesState}

case class IdealGasFrameLog[V <: AlgebraicVector[V], M[_] : Monad](
                                                                    override val particles: ParticlesState[V, M],
                                                                    override val particlesReducer: ParticlesReducer[V, M],
                                                                  ) extends FrameLog[V, M] {

  override protected def initActions: Seq[ParticlesChangeAction[V]] = nextActions

  override def nextActions: Seq[ParticlesChangeAction[V]] = {
    makeNewParticlesAnsamble[V]
  }

  override protected def updateWithParticles(particles: ParticlesState[V, M]): FrameLog[V, M] = this.copy(particles = particles)

  protected def makeNewParticlesAnsamble[V <: AlgebraicVector[V]](): Seq[ParticlesChangeAction[V]] = {
    Seq.concat(
      Seq(), // positions
      Seq(), // velocities
    )
    Seq()
  }
}
