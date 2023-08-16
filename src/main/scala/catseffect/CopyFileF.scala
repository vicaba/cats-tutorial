package catseffect

import cats.effect._
import java.io._
import cats.syntax.all._

// https://blog.craftlab.hu/tagless-final-from-a-different-perspective-72321a29bbe3
object CopyFileF extends IOApp {

  def transmit[F[_]: Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.length))
      count  <- if(amount > -1) Sync[F].blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
      else Sync[F].pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
    } yield count // Returns the actual amount of bytes transmitted

  def transfer[F[_] : Sync](origin: InputStream, destination: OutputStream): F[Long] =
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)

/*  private def openStream[F[_]: Sync, A](f: File, openStream: () => A): F[A] =
    Sync[F].blocking(openStream()).handleErrorWith(_ => Sync[F].delay(println(s"Error opening ${f.getPath}"))) // release*/
  private def closeStream[F[_]: Sync](f: File, stream: Closeable): F[Unit] =
    Sync[F].blocking(stream.close()).handleErrorWith(_ => Sync[F].delay(println(s"Error closing ${f.getPath}"))) // release

  def inputStream[F[_] : Sync](f: File): Resource[F, FileInputStream] = Resource.make {
    Sync[F].blocking(new FileInputStream(f)) // build
  } { closeStream[F](f, _)}

  def outputStream[F[_] : Sync](f: File): Resource[F, FileOutputStream] = Resource.make {
    Sync[F].blocking(new FileOutputStream(f)) // build
  } {closeStream[F](f, _)}

  def inputOutputStreams[F[_] : Sync](in: File, out: File): Resource[F, (InputStream, OutputStream)] = for {
    inStream <- inputStream(in)
    outStream <- outputStream(out)
  } yield (inStream, outStream)

  def copy[F[_] : Sync](origin: File, destination: File): F[Long] =
    inputOutputStreams(origin, destination).use((in, out) => transfer(in, out))

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
      else IO.unit
      _ <- if (args(0) == args(1)) IO.raiseError(new IllegalArgumentException("Origin and destination cannot be the same"))
      else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      count <- copy[IO](orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}
