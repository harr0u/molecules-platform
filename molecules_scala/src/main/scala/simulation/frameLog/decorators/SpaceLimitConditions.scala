package simulation.frameLog.decorators

import calculation.space.SpaceConditions
import cats.Traverse
import domain.geometry.figures.GeometricFigure
import domain.geometry.vector.AlgebraicVector
import simulation.actions.{ParticlesChangeAction, UpdatePositions}
import simulation.frameLog.FrameLog

trait SpaceLimitConditions[V <: AlgebraicVector[V], Fig <: GeometricFigure, Context[_], T[_]] extends FrameLog[V, Context, T] {
  def spaceConditions: SpaceConditions[V, Fig]

  abstract override def initActions: Context[Seq[ParticlesChangeAction[V]]] = {
    Context.map(super.initActions)(spaceLimitConditionsAction +: _)
  }

  abstract override def nextActions: Context[Seq[ParticlesChangeAction[V]]] = {
    Context.map(super.nextActions)(spaceLimitConditionsAction +: _)
  }

  protected def spaceLimitConditionsAction: ParticlesChangeAction[V] = {
    UpdatePositions[V](p => spaceConditions.positionLimitCondition(p.position))
  }
}
