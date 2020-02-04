package molecules

import domain.Particle
import domain.vector._

sealed trait ParticlesChangeAction[V <: AlgebraicVector[V]]

case class UpdatePositions[V <: AlgebraicVector[V]](fn: (Particle[V]) => V) extends ParticlesChangeAction[V]
case class UpdateVelocities[V <: AlgebraicVector[V]](fn: (Particle[V]) => V) extends ParticlesChangeAction[V]
case class ZeroForces[V <: AlgebraicVector[V]]() extends ParticlesChangeAction[V]
case class ZeroPotentials[V <: AlgebraicVector[V]]() extends ParticlesChangeAction[V]
case class UpdateForceAndPotential[V <: AlgebraicVector[V]](
                                                             fn: (Particle[V], Particle[V]) => (Particle[V], Particle[V])
) extends ParticlesChangeAction[V]


abstract class ParticlesAbstractContainer[V <: AlgebraicVector[V]] {
    def applyChangeAction(action: ParticlesChangeAction[V]): ParticlesAbstractContainer[V]
    // Actions should be applied consistently
    def applyChangeActions(actions: Seq[ParticlesChangeAction[V]]): ParticlesAbstractContainer[V]

    def particlesStream: LazyList[Particle[V]]
    def particlePairsStream: LazyList[(Particle[V], Particle[V])]
}
