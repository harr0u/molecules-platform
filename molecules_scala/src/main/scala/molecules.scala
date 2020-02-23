import java.io.{BufferedWriter, File, FileWriter}

import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.limitConditions.SpaceConditions
import domain.Particle
import domain.geometry.vector._
import calculation.numerical.LeapFrogIteration
import calculation.physics.{CenterOfMassCalculator, LennardJonesCutOffPotential, LennardJonesPotential}
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import state.cells.{PeriodicFutureParticlesCells2D, PeriodicFutureParticlesCells3D}
import state.{ParticlesSeqState, ParticlesState, ParticlesStateReducer}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random


object Molecules extends App {

}