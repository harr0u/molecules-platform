package e2e

import calculation.limitConditions.SpaceConditions
import calculation.limitConditions.periodic.{BoxPeriodicSpaceConditions, RectanglePeriodicSpaceConditions}
import calculation.numerical.{FrameLog, LeapFrogIteration}
import calculation.physics.{LennardJonesPotential, PotentialCalculator}
import cats.{Applicative, Functor, Monad, Traverse}
import domain.Particle
import domain.geometry.figures.{Cube, CubicFigure, GeometricFigure, RectangleFigure, Square}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{FutureMatchers, MatchResult, Matcher}
import state.{ParticlesSeqState, ParticlesState, ParticlesStateReducer}
import state.cells.{PeriodicFutureParticlesCells2D, PeriodicFutureParticlesCells3D}

import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import org.specs2._
import org.specs2.specification.AllExpectations



trait FrameLogTester {
  this: mutable.Specification with FutureMatchers =>

  def meanSquaredErrorOfTotalEnergy[V <: AlgebraicVector[V], Fig <: GeometricFigure](
    particles: ParticlesState[V, Future],
    box: SpaceConditions[V, Fig],
    numberOfFrames: Int,
    expectedNumberOfParticles: Int,
    `∆t`: Double = 0.001,
    verbose: Option[Int] = Some(100)
  )(implicit ee: ExecutionEnv, potential: PotentialCalculator[V]): Future[Double] = {

    implicit val m: Monad[Future] = FrameLogTester.monadInstanceForFuture
    val frameLog: FrameLog[V, Fig, Future] = new FrameLog[V, Fig, Future](
      particles,
      new ParticlesStateReducer[V, Future],
      box,
      new LeapFrogIteration[V, Fig],
      `∆t` = `∆t`
    )

    frameLog.particles.counit.length must_=== expectedNumberOfParticles

    val particlesLog: LazyList[Future[FrameLog[V, Fig, Future]]] = LazyList.iterate(frameLog.init)(
      iF => iF.flatMap(iteration => iteration.next)
    )

    val framesHistory: Future[LazyList[(Double, Long)]] = Future.sequence(
      buildTotalEnergyLog(particlesLog, verbose).take(numberOfFrames)
    )

    framesHistory.map(framesEnergy => {
      val energySeq: Seq[Double] = framesEnergy.map(_._1)
      val average = energySeq.iterator.sum / numberOfFrames

      energySeq.map(e => Math.pow(e - average, 2)).iterator.sum / numberOfFrames
    })
  }

  def buildTotalEnergyLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, F[_]](moleculesLog: LazyList[F[FrameLog[V, Fig, F]]], verbose: Option[Int] = None)
                                                                        (implicit ec: ExecutionContext, F : Functor[F]): LazyList[F[(Double, Long)]] = {
    val log = (index: Int, energies: (Double, Double, Double)) => if (verbose.exists(x => index % x == 0)) {
      val (total, potential, kineticEnergy) = energies
      println(f"Frame ${index} - T[${total}] - P[${potential}] - K[${kineticEnergy}]")
    }

    moleculesLog.zipWithIndex.map {
      case (iter: F[FrameLog[V, Fig, F]], i: Int) => F.map(iter)((iteration) => {
        val numberOfParticles = iteration.particles.counit.length
        val kineticEnergy: Double = iteration.particles.counit.map((p) => p.velocity.squaredLength / 2).iterator.sum / numberOfParticles
        val potential: Double = iteration.particles.counit.map(_.potential).iterator.sum / numberOfParticles
        val total: Double = kineticEnergy + potential

        log(i, (total, potential, kineticEnergy))

        (total, System.currentTimeMillis)
      })
    }
  }

  def makeParticlesIn(box: CubicFigure, sideNumber: Int, velocityFactor: Double): Seq[Particle[Vector3D]] = {
    val rng = new Random(0L)

    for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
      k <- 0 until sideNumber
    } yield {
      new Particle[Vector3D](
        i * sideNumber * sideNumber + j * sideNumber + k,
        Vector3D((i + 0.5) * box.width / sideNumber, (j + 0.5) * box.width / sideNumber, (k + 0.5) * box.width / sideNumber),
        Vector3D(rng.nextDouble() - 0.5, rng.nextDouble() - 0.5, rng.nextDouble() - 0.5) * velocityFactor,
        Vector3D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }
  }

  def makeParticlesIn(box: RectangleFigure, sideNumber: Int, velocityFactor: Double): Seq[Particle[Vector2D]] = {
    val rng = new Random(0L)

    for {
      i <- 0 until sideNumber
      j <- 0 until sideNumber
    } yield {
      new Particle[Vector2D](
        i * sideNumber + j,
        Vector2D((i + 0.5) * box.width / sideNumber, (j + 0.5) * box.length / sideNumber),
        Vector2D(rng.nextDouble() - 0.5, rng.nextDouble() - 0.5) * velocityFactor,
        Vector2D.empty,
        potential = 0.0,
        mass = 1.0
      )
    }
  }
}

object FrameLogTester {
  import scala.concurrent.ExecutionContext.Implicits.global;

  implicit val monadInstanceForFuture: Monad[Future] = new Monad[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.flatMap(a => pure(f(a)))

    override def pure[A](x: A): Future[A] = Future.successful(x)

    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]): Future[B] = {
      pure(a)
        .flatMap(f)
        .flatMap {
          case Left(v) => tailRecM(v)(f)
          case Right(b) => Future.successful(b)
        }
    }
  }
}
