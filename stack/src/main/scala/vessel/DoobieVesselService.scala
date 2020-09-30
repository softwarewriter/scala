package vessel

import cats.effect.{Bracket}
import cats.free.Free
import cats.implicits._
import doobie._
import doobie.free.connection
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.util.compat.FactoryCompat

/**
 * Implementation of [[VesselService]] that targets doobie.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class DoobieVesselService[F[_]](val transactor: Transactor[F])(implicit B: Bracket[F, Throwable]) extends VesselService[F] {

   private implicit def listFactoryCompat[A]: FactoryCompat[A, List[A]] = FactoryCompat.fromFactor(List.iterableFactory)

   override def get(imo: String): F[Option[Vessel]] = {
      val statement: Fragment = sql"select imo from vessel where imo = '$imo'"
      val query: Query0[String] = statement.query[String]

     query.to[List]
     val result: Free[connection.ConnectionOp, Option[Vessel]] = query.option.map(maybeImo => maybeImo.map(imo => Vessel(imo, "s")))
     val r2: F[Option[Vessel]] = result.transact(transactor)
     r2
   }

  // onError.



      /*{
      val statement: Fragment = sql"select imo, name from vessel"
      val query: Query0[Vessel] = statement.query[Vessel]
      val list: ConnectionIO[List[String]] = query.to[List]
      val transaction: F[List[String]] = list.transact(transactor)

   }
   */

   override def put(vessel: Vessel): F[Vessel] = ???

   override def delete(imo: String): F[Option[Vessel]] = ???

   override def search(query: String): F[List[Vessel]] = ???

}

/*
  import doobie._
  import doobie.implicits._
  val statement: Fragment = sql"select name, IMO from vessel" //fragments can compose to create a "full" sql query
  val query: Query0[Vessel] = statement.query[Vessel] //.map{case (name, imo) => Vessel(name, imo)} //When .query bind result type and we get a query
  val lst: ConnectionIO[List[String]] = query.to[List] // specify what cardinality to expect of the result type, may now be composed with other queries inside a transaction
  val transaction: F[List[String]] = lst.transact(transactor) //takes the composed connectionIO(s) and creates a transaction boundary

 */
