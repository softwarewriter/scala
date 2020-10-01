package vessel

import cats.effect.Bracket
import cats.free.Free
import cats.implicits._
import doobie._
import doobie.free.connection
import doobie.implicits._
import doobie.util.compat.FactoryCompat

import scala.None

/**
 * Implementation of [[VesselService]] that targets doobie.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class DoobieVesselService[F[_]](val transactor: Transactor[F])(implicit B: Bracket[F, Throwable]) extends VesselService[F] {

   private implicit def listFactoryCompat[A]: FactoryCompat[A, List[A]] = FactoryCompat.fromFactor(List.iterableFactory)

   override def get(imo: String): F[Option[Vessel]] = {
      Connection.get(imo).transact(transactor)
   }

   override def put(vessel: Vessel): F[Vessel] = {
      /*
       Implemented as if update sql does not exists.
       */
      Connection.get(vessel.imo).flatMap {
         case Some(_) =>
            Connection.delete(vessel.imo) *> Connection.insert(vessel)
         case None =>
            Connection.insert(vessel)
      }.map(_ => vessel).transact(transactor)
   }

   override def delete(imo: String): F[Option[Vessel]] = {
      val v1: ConnectionIO[Option[Vessel]] = Connection.get(imo).flatMap{
         case Some(vessel) =>
            Connection.delete(imo).map(_ => Option(vessel))
         case None =>
            "no-operation".pure[ConnectionIO].map(_ => None)
      }
      v1.transact(transactor)
   }

   override def search(queryString: String): F[List[Vessel]] = {
      val queryStringWithWildcards = s"%$queryString%"
      val statement: Fragment = sql"""select imo, name from vessel where imo like $queryStringWithWildcards or name like $queryStringWithWildcards"""
      val query: Query0[Vessel] = statement.query[Vessel]
      query.to[List].transact(transactor)
   }

  object Connection {

     def get(imo: String): ConnectionIO[Option[Vessel]] = {
        val statement: Fragment = sql"""select imo, name from vessel where imo = $imo"""
        val query: Query0[Vessel] = statement.query[Vessel]
        query.option
     }

     def insert(vessel: Vessel): ConnectionIO[Int] = {
        val statement: Fragment = sql"""insert into vessel (imo, name) values (${vessel.imo}, ${vessel.name})"""
        val update: Update0 = statement.update
        update.run
     }

     def delete(imo: String): ConnectionIO[Int] = {
        val statement: Fragment = sql"""delete from vessel where imo = $imo"""
        val update: Update0 = statement.update
        update.run
     }

  }


  // DoobieChecker i Vessel api.

  // GitLab Kan du høre med Alekxander Koch på Intility kanalen?
  object Q {

  }


}

/*
  import doobie._
  import doobie.implicits._
  val statement: Fragment = sql"select name, IMO from vessel" //fragments can compose to create a "full" sql query
  val query: Query0[Vessel] = statement.query[Vessel] //.map{case (name, imo) => Vessel(name, imo)} //When .query bind result type and we get a query
  val lst: ConnectionIO[List[String]] = query.to[List] // specify what cardinality to expect of the result type, may now be composed with other queries inside a transaction
  val transaction: F[List[String]] = lst.transact(transactor) //takes the composed connectionIO(s) and creates a transaction boundary

 */
