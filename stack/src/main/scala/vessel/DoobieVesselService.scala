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

   /*
       val statement: Fragment = sql"""select imo, name from vessel where imo = $imo"""
       val query: Query0[(String, String)] = statement.query[(String, String)]

 //     query.to[List]
      val result: Free[connection.ConnectionOp, Option[Vessel]] = query.option.map(maybeRow => maybeRow.map(row => Vessel(row._1, row._2)))
      val r2: F[Option[Vessel]] = result.transact(transactor)
      r2

    */



   // onError.



   /*{
   val statement: Fragment = sql"select imo, name from vessel"
   val query: Query0[Vessel] = statement.query[Vessel]
   val list: ConnectionIO[List[String]] = query.to[List]
   val transaction: F[List[String]] = list.transact(transactor)

}
*/

   override def get(imo: String): F[Option[Vessel]] = {
      getConnection(imo).transact(transactor)
   }

   override def put(vessel: Vessel): F[Vessel] = {
      /*
       Implemented as update sql does not exists.
       */
      getConnection(vessel.imo).flatMap {
         case Some(_) => {
            deleteConnection(vessel.imo).flatMap(_ => insertConnection(vessel))
         }
         case None => {
            insertConnection(vessel)
         }
      }.map(_ => vessel).transact(transactor)
   }

   override def delete(imo: String): F[Option[Vessel]] = {
      getConnection(imo).flatMap(maybeVessel => maybeVessel match {
         case Some(vessel) => {
            deleteConnection(imo)
         }
         case None => {
            deleteConnection(imo)
         }
      }).map(deletedRows => if (deletedRows == 0) None else Some(Vessel(imo, "pelle"))).transact(transactor)
   }

   override def search(queryString: String): F[List[Vessel]] = {
      val queryStringWithWildcards = s"%$queryString%"
      val statement: Fragment = sql"""select imo, name from vessel where imo like $queryStringWithWildcards or name like $queryStringWithWildcards"""
      val query: Query0[Vessel] = statement.query[Vessel]
      query.to[List].transact(transactor)
   }

   private def getConnection(imo: String): ConnectionIO[Option[Vessel]] = {
      val statement: Fragment = sql"""select imo, name from vessel where imo = $imo"""
      val query: Query0[Vessel] = statement.query[Vessel]
      query.option
   }

   private def insertConnection(vessel: Vessel): ConnectionIO[Int] = {
      val statement: Fragment = sql"""insert into vessel (imo, name) values (${vessel.imo}, ${vessel.name})"""
      val update: Update0 = statement.update
      update.run
   }

   private def deleteConnection(imo: String): ConnectionIO[Int] = {
      val statement: Fragment = sql"""delete from vessel where imo = $imo"""
      val update: Update0 = statement.update
      update.run
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
