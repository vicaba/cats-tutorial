package rockthejvm

// https://www.youtube.com/watch?v=TBe0_iVOYSM&t=2s

import cats.effect.kernel.Fiber
import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration.*

/**
 * Fibers are lightweight threads
 */

object AsynchronousIOs extends IOApp {

  val meaningOfLife: IO[Int] = IO(42)
  val favLang: IO[String] = IO("Scala")

  /**
   * Fiber is a generic type. IO is the effect type,
   * Throwable the type returned when failing,
   * String the type when successful
   * @return
   */
  def createFiber: Fiber[IO, Throwable, String] = ???

  extension [A] (io: IO[A])
    def myDebug: IO[A] = io.map { value =>
      println(s"${Thread.currentThread().getName} $value")
      value
    }

  def sameThread(): IO[Unit] = for {
    _ <- meaningOfLife.myDebug
    _ <- favLang.myDebug
  } yield ()

  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.myDebug.start

  def differentThreads() = for {
    _ <- aFiber.myDebug
    _ <- favLang.myDebug
  } yield ()

  def runOnAnotherThread[A](io: IO[A]) = for {
    fib <- io.start
    result <- fib.join
  } yield result

  def throwOnAnotherThread[A](io: IO[A]) = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you!")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").myDebug *> IO.sleep(1.second) *> IO("done").myDebug
    for {
      fib <- task.start
      _ <- IO.sleep(500.millis) *> IO("cancelling").myDebug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  override def run(args: List[String]): IO[ExitCode] = testCancel().myDebug.as(ExitCode.Success)
}
