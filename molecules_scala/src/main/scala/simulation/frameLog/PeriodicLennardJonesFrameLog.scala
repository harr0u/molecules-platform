package simulation.frameLog

import calculation.physics.potentials.PairwisePotentialCalculator
import calculation.space.SpaceConditions
import cats.Monad
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState
import simulation.actions.ParticlesChangeAction
import simulation.frameLog.decorators.{CenterOfMassReferentialFrame, LeapFrogIteration, SpaceLimitConditions}
import simulation.reducers.ParticlesStateReducer

case class PeriodicLennardJonesFrameLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, M[_] : Monad](
                                                                                                     override val particles: ParticlesState[V, M],
                                                                                                     override val particlesReducer: ParticlesStateReducer[V, M],
                                                                                                     override val spaceConditions: SpaceConditions[V, Fig],
                                                                                                     override val `∆t`: Double = 0.0001
                                                                                                   )(implicit pc: PairwisePotentialCalculator[V]) extends FrameLog[V, M]
  with CenterOfMassReferentialFrame[V, M]
  with LeapFrogIteration[V, M]
  with SpaceLimitConditions[V, Fig, M] {

  override val potentialCalculator: PairwisePotentialCalculator[V] = pc

  override def updateWithParticles(particles: ParticlesState[V, M]): FrameLog[V, M] = this.copy()
}
