package catseffect

import java.io.*
import cats.effect.*

import scala.annotation.targetName
import scala.util.Try

// https://typelevel.org/cats-effect/docs/tutorial
object CopyFile extends IOApp {

  private def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      count <- if (amount > -1) IO.blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted // Returns the actual amount of bytes transmitted

  private def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

  private def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f)) // build
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.println(s"Error closing ${f.getPath}")) // release
    }

  private def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f)) // build
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.println(s"Error closing ${f.getPath}")) // release
    }

  private def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)


  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

  private def isWriteToDestFile(f: File): IO[Boolean] = {
    def chooseDestFileOverwrite(file: File): IO[Boolean] = for {
      answer <- IO.println(s"Do you want to overwrite the file ${f.getPath}? (1/2)") >> IO.readLine
      answerBool <- answer match
        case "1" => IO.pure(true)
        case "2" => IO.pure(false)
        case _ => IO.println("Please, select 1 or 2") >> chooseDestFileOverwrite(f)
    } yield answerBool

    for {
      fileExists <- IO.delay(f.exists())
      writeFile <- if (fileExists) chooseDestFileOverwrite(f) else IO.pure(true)
    } yield writeFile
  }

  private def checkArguments(args: List[String]): IO[Unit] = for {
    _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
    else IO.unit
    _ <- if (args(0) == args(1)) IO.raiseError(new IllegalArgumentException("Origin and destination cannot be the same"))
    else IO.unit
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- checkArguments(args)
      orig = new File(args(0))
      dest = new File(args(1))
      writeFile <- isWriteToDestFile(dest)
      count <- if (writeFile) copy(orig, dest) else IO.pure(0)
      _ <- if (count != 0)
        IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
      else
        IO.println("No bytes copied")
    } yield ExitCode.Success

}
