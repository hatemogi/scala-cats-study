package study.cats

import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}

object IOTrial extends App {

  def p(s: String): Unit = {
    println(s"[${Thread.currentThread.getName}] $s")
  }

  val ioa = IO { p("hey!") }

  implicit val ec: ExecutionContext = ExecutionContext.global

  val async = IO.async { (cb: Either[Throwable, String] => Unit) =>
    Future {
      p("async start")
      Thread.sleep(1000)
      p("after 1sec")
      cb(Right("success"))
    }
  }

  val program: IO[Unit] =
    for {
      _ <- ioa
      _ <- ioa
      str <- async
      _ <- IO { p(str) }
    } yield ()

  program.unsafeRunSync()
}
