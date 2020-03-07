package simulation.frameLog

import calculation.physics.potentials.PairwisePotentialCalculator
import calculation.space.SpaceConditions
import cats.{Monad, Traverse}
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector
import simulation.ParticlesState
import simulation.actions.ParticlesChangeAction
import simulation.frameLog.decorators.{CenterOfMassReferentialFrame, LeapFrogIteration, SpaceLimitConditions}
import simulation.reducers.ParticlesStateReducer

case class PeriodicLennardJonesFrameLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, Context[_] : Monad, T[_]: Traverse](
                                                                                                     override val particles: ParticlesState[V, Context, T],
                                                                                                     override val particlesReducer: ParticlesStateReducer[V, Context, T],
                                                                                                     override val spaceConditions: SpaceConditions[V, Fig],
                                                                                                     override val `∆t`: Double = 0.0001
                                                                                                   )(implicit pc: PairwisePotentialCalculator[V]) extends FrameLog[V, Context, T]
  with CenterOfMassReferentialFrame[V, Context, T]
  with LeapFrogIteration[V, Context, T]
  with SpaceLimitConditions[V, Fig, Context, T] {

  override val potentialCalculator: PairwisePotentialCalculator[V] = pc

  override def updateWithParticles(particles: ParticlesState[V, Context, T]): FrameLog[V, Context, T] = this.copy()
}
