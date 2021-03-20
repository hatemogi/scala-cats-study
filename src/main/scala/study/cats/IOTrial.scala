package study.cats

import cats.effect.{ContextShift, IO}

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

  def fib(n: Int, a: Long, b: Long): IO[Long] =
    IO.suspend {
      if (n > 0)
        fib(n - 1, b, a + b)
      else
        IO.pure(a)
    }

  def fib2(n: Int, a: Long, b: Long)(implicit cs: ContextShift[IO]): IO[Long] =
    IO.suspend {
      if (n == 0) IO.pure(a) else {
        val next = fib(n - 1, b, a + b)
        // Every 100 cycles, introduce a logical thread fork
        if (n % 100 == 0)
          cs.shift *> next
        else
          next
      }
    }

  val program: IO[Unit] =
    for {
      _ <- ioa
      _ <- ioa
      str <- async
      _ <- IO { p(str) }
      f10 <- fib(10, 0, 1)
      _ <- IO { p(s"fib(10) = $f10") }
      f11 <- fib(20, 0, 1)
      _ <- IO { p(s"fib(20) = $f11") }
    } yield ()

  program.unsafeRunSync()
}
