package study.cats

import cats._
import cats.implicits._

object DataTypes extends App {

  def semigroupal(): Unit = {
    val oa = Option[String]("abc")
    val ob = Option[String]("def")

    println(Semigroupal[Option].product(oa, ob))

    println((oa, ob).tupled)

    println(for {
      a <- oa
      b <- ob
    } yield (a, b))
  }

  def applicative(): Unit = {
    val fa = Option[String]("abc")
    val f: String => Int = _.length
    val ff: Option[String => Int] = Some(f)

    val ys: List[Option[Int]] = List(
      for { a <- fa  } yield f(a),
      fa map f,
      Applicative[Option].ap(ff)(fa),
      ff ap fa,
      ff <*> fa
    )
    println(ys forall { _ contains 3 })
  }

  def applicative2(): Unit = {
    val fa = Option[String]("abc")
    val fb = Option[Int](3)
    val f: (String, Int) => Boolean = (s, i) => s.length == i
    val ff: Option[(String, Int) => Boolean] = Some(f)

    val ys: List[Option[Boolean]] = List(
      for {
        a <- fa
        b <- fb
      } yield f(a, b),
      Applicative[Option].ap2(ff)(fa, fb),
      ff.ap2(fa, fb),
      (fa, fb) apWith ff,
      Apply[Option].map2(fa, fb)(f),
      (fa, fb) mapN f,
    )
    println(ys forall { _ contains true })
  }

  //semigroupal()
  applicative()
  applicative2()
}
