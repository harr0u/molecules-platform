package domain

import domain.geometry.figures.{CubicFigure, GeometricFigure, RectangleFigure, Sphere}
import domain.geometry.vector.AlgebraicVector

object ParticlesFactory {
  def uniformEnsemble[V <: AlgebraicVector[V]](box: GeometricFigure, _count: Int): Seq[Particle[V]] = ???
}
