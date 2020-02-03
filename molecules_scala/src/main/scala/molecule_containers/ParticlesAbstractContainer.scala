package molecules

import domain.Particle
import domain.vector._

abstract class ParticlesAbstractContainer [VectorType <: AlgebraicVector[VectorType]] {
    def particlesStream: LazyList[Particle[VectorType]]
    def particlePairsStream: LazyList[(Particle[VectorType], Particle[VectorType])]
}
