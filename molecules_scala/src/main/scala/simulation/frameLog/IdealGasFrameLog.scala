package simulation.frameLog
import cats.{Id, Monad}
import domain.geometry.vector.AlgebraicVector
import simulation.actions.ParticlesChangeAction
import simulation.state.ParticlesListState
import simulation.{ParticlesReducer, ParticlesState}

case class IdealGasFrameLog[V <: AlgebraicVector[V], M[_] : Monad](
                                                                    override val particles: ParticlesState[V, M, List],
                                                                    override val particlesReducer: ParticlesReducer[V, M, List],
                                                                  ) extends FrameLog[V, M, List] {

  override protected def initActions: Seq[ParticlesChangeAction[V]] = makeNewParticlesAnsamble

  override def nextActions: Seq[ParticlesChangeAction[V]] = initActions

  override protected def updateWithParticles(particles: ParticlesState[V, M, List]): FrameLog[V, M, List] = {
    this.copy(particles = particles)
  }

  protected def makeNewParticlesAnsamble: Seq[ParticlesChangeAction[V]] = {
    Seq.concat(
      Seq(), // positions
      Seq(), // velocities
    )
    Seq()
  }
}
