package e2e

import calculation.limitConditions.SpaceConditions
import calculation.numerical.{FrameLog, LeapFrogIteration}
import calculation.physics.{LennardJonesPotential, PotentialCalculator}
import cats.{Functor, Monad, Traverse}
import domain.Particle
import domain.geometry.figures.{CubicFigure, GeometricFigure, RectangleFigure}
import domain.geometry.vector.{AlgebraicVector, Vector2D, Vector3D}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.FutureMatchers
import simulation.ParticlesState
import simulation.reducers.ParticlesStateReducer
//import scala.concurrent.ExecutionContext.Implicits.global
import org.specs2._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random
import cats.implicits._


trait FrameLogTester {
  this: mutable.Specification with FutureMatchers =>

  def meanSquaredErrorOfTotalEnergy[V <: AlgebraicVector[V], Fig <: GeometricFigure, F[_]](
    particles: ParticlesState[V, F],
    box: SpaceConditions[V, Fig],
    numberOfFrames : Int,
    `∆t`: Double = 0.001,
    verbose: Option[Int] = Some(100)
  )(implicit ee: ExecutionEnv, potential: LennardJonesPotential[V], F : Monad[F], SeqForF : Traverse[List]): F[Double] = {

    val frameLog: FrameLog[V, Fig, F] = new FrameLog[V, Fig, F](
      particles,
      new ParticlesStateReducer[V, F],
      box,
      new LeapFrogIteration[V, Fig],
      `∆t` = `∆t`
    )

    val particlesLog: List[F[FrameLog[V, Fig, F]]] = List.iterate(frameLog.init, numberOfFrames)(
      iF => iF.flatMap(iteration => iteration.next)
    )

    val framesHistory: F[List[(Double, Long)]] = SeqForF.sequence(
      buildTotalEnergyLog(particlesLog, verbose).take(numberOfFrames)
    )

    framesHistory.map(framesEnergy => {
      val energySeq: List[Double] = framesEnergy.map(_._1)
      val average = energySeq.iterator.sum / numberOfFrames

      energySeq.map(e => Math.pow(e - average, 2)).iterator.sum / numberOfFrames
    })
  }

  def buildTotalEnergyLog[V <: AlgebraicVector[V], Fig <: GeometricFigure, F[_] : Functor](moleculesLog: List[F[FrameLog[V, Fig, F]]], verbose: Option[Int] = None)
                                                                        (implicit ec: ExecutionContext): List[F[(Double, Long)]] = {
    val log = (index: Int, energies: (Double, Double, Double)) => if (verbose.exists(x => index % x == 0)) {
      val (total, potential, kineticEnergy) = energies
      println(f"Frame ${index} - T[${total}] - P[${potential}] - K[${kineticEnergy}]")
    }

    moleculesLog.zipWithIndex.map {
      case (iter, i: Int) => iter.map((iteration) => {
        val numberOfParticles = iteration.particles.getParticles.length
        val kineticEnergy: Double = iteration.particles.getParticles.map((p) => p.velocity.squaredLength / 2).iterator.sum / numberOfParticles
        val potential: Double = iteration.particles.getParticles.map(_.potential).iterator.sum / numberOfParticles
        val total: Double = kineticEnergy + potential

        log(i, (total, potential, kineticEnergy))

        (total, System.currentTimeMillis)
      })
    }
  }

  def makeParticlesIn(box: CubicFigure, sideNumber: Int, velocityFactor: Double): List[Particle[Vector3D]] = {
    val rng = new Random(0L)

    val particlesSeq = for {
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

    particlesSeq.toList
  }

  def makeParticlesIn(box: RectangleFigure, sideNumber: Int, velocityFactor: Double): List[Particle[Vector2D]] = {
    val rng = new Random(0L)

    val particlesSeq = for {
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

    particlesSeq.toList
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
