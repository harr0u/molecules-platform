package simulation.frameLog.decorators

import calculation.space.SpaceConditions
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdatePositions}
import simulation.frameLog.FrameLog

trait SpaceLimitConditions[V <: AlgebraicVector[V], Fig <: GeometricFigure, M[_]] extends FrameLog[V, M] {
  def spaceConditions: SpaceConditions[V, Fig]

  abstract override def initActions: Seq[ParticlesChangeAction[V]] = {
    spaceLimitConditionsAction +: super.initActions
  }

  abstract override def nextActions: Seq[ParticlesChangeAction[V]] = {
    spaceLimitConditionsAction +: super.nextActions
  }

  protected def spaceLimitConditionsAction: ParticlesChangeAction[V] = {
    UpdatePositions[V](p => spaceConditions.positionLimitCondition(p.position))
  }
}