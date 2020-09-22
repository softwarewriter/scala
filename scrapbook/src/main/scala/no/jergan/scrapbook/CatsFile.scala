package no.jergan.scrapbook

import cats.effect.{IO, Resource}
import cats.implicits._
import java.io._

/**
 * Test of cats.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object CatsFile {


   def main(args: Array[String]): Unit = {

      def inputStream(f: File): Resource[IO, FileInputStream] =
         Resource.make {
            IO(new FileInputStream(f))                         // build
         } { inStream =>
            IO(inStream.close()).handleErrorWith(_ => IO.unit) // release
         }

      def outputStream(f: File): Resource[IO, FileOutputStream] =
         Resource.make {
            IO(new FileOutputStream(f))                         // build
         } { outStream =>
            IO(outStream.close()).handleErrorWith(_ => IO.unit) // release
         }

      def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
         for {
            inStream  <- inputStream(in)
            outStream <- outputStream(out)
         } yield (inStream, outStream)

      def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
         for {
            amount <- IO(origin.read(buffer, 0, buffer.size))
            count  <- if(amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
            else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
         } yield count // Returns the actual amount of bytes transmitted

      def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
         for {
            buffer <- IO(new Array[Byte](1024 * 10)) // Allocated only when the IO is evaluated
            total  <- transmit(origin, destination, buffer, 0L)
         } yield total

      def copy(origin: File, destination: File): IO[Long] =
         inputOutputStreams(origin, destination).use { case (in, out) =>
            transfer(in, out)
         }

      val in = new File("/local1/jergan/download/pelle.txt")
      val out = new File("/local1/jergan/download/pelle2.txt")

      val theCopy = copy(in, out)

      val listener: Either[Throwable, Long] => Unit = (result) => {
         (result) match {
            case Left(exception) => println(exception.getMessage)
            case Right(value) => println("Copied: " + value)
         }
      }
      val result = theCopy.unsafeRunSync()
      println("sync result: " + result);


      theCopy.unsafeRunAsync(listener)

      def myPrint(a: String): Unit = {
         println(a)
      }

      val hei = IO(myPrint("hei"))
      hei.unsafeRunSync()
      hei.unsafeRunSync()
   }

}
