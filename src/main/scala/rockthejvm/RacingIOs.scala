package rockthejvm

// https://www.youtube.com/watch?v=75HqyZ04AFs&t=349s min 5

import cats.effect.{IO, IOApp}
import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {
  val meaningOfLife: IO[Int] = IO(42)

  extension [A] (io: IO[A]) {
    def myDebug: IO[A] = io.map { value =>
      println(s"${Thread.currentThread().getName} $value")
      value
    }
  }

  val valueableIO: IO[Int] = IO("task starting").myDebug *> IO.sleep(1.second) *> IO("task: completed").myDebug *> IO(42)
  val vIO: IO[Int] = valueableIO.onCancel(IO("task: cancelled").myDebug.void)
  val timeout: IO[Unit] = IO("timeout: starting").myDebug *> IO.sleep(500.millis) *> IO("timeout: DING DING!").myDebug.void

  // Racing
  def testRece() = {
    val firstIO: IO[Either[Int, Unit]] = IO.race(vIO, timeout)
    firstIO.flatMap {
      case Left(v) => IO(s"Task won: $v")
      case Right(_) => IO("Timeout won")
    }
  }

  val testTimeout: IO[Int] = vIO.timeout(500.millis)

  // RacePair
  def demoRacePair[A](iox: IO[A], ioy: IO[A]) = {
    val pair = IO.racePair(iox, ioy)  // IO[Either[(OutcomeIO[A], FiberIO[B]), (OutcomeIO[B], FiberIO[A])]]
    pair.flatMap {
      case Left((outcomeA, fiberB)) => fiberB.cancel *> IO("first task won").myDebug *> IO(outcomeA).myDebug
      case Right((fiberA, outcomeB)) => fiberA.cancel *> IO("second task won").myDebug *> IO(outcomeB).myDebug
    }
  }

  val iox = IO.sleep(1.second).as(1).onCancel(IO("first cancelled").myDebug.void)
  val ioy = IO.sleep(2.second).as(2).onCancel(IO("second cancelled").myDebug.void)


  override def run: IO[Unit] = demoRacePair(iox, ioy).void
}

