package no.jergan.scrapbook.scalawithcats

import cats.Applicative
import cats.data.Reader

object Ex483 {

  final case class Db(
                       usernames: Map[Int, String],
                       passwords: Map[String, String]
                     )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    new DbReader[Option[String]](db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    new DbReader[Boolean](db => db.passwords.get(username).exists(_.equals(password)))

  def checkLogin1(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId)
      .flatMap {
        case Some(un) => checkPassword(un, password)
        case None => Applicative[DbReader].pure(false)
      }

  def checkLogin2(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      ok <- username.map(un => checkPassword(un, password)).getOrElse(Applicative[DbReader].pure(false))
    } yield ok

  def main(args: Array[String]): Unit = {
  }

}
