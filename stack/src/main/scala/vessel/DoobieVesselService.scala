package vessel

import cats.effect.Bracket
import doobie._
import doobie.implicits._

/**
 * Implementation of [[VesselService]] that targets doobie.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class DoobieVesselService[F[_]](val transactor: Transactor[F])(implicit B:Bracket[F,Throwable]) extends VesselService {

   override def get(imo: String): Option[Vessel] = {
      println(transactor)


      Some(Vessel("1", "Titanic"))
   }


      /*{
      val statement: Fragment = sql"select imo, name from vessel"
      val query: Query0[Vessel] = statement.query[Vessel]
      val list: ConnectionIO[List[String]] = query.to[List]
      val transaction: F[List[String]] = list.transact(transactor)

   }
   */

   override def put(vessel: Vessel): Vessel = ???

   override def delete(imo: String): Option[Vessel] = ???

   override def search(query: String): List[Vessel] = ???

}

/*
  import doobie._
  import doobie.implicits._
  val statement: Fragment = sql"select name, IMO from vessel" //fragments can compose to create a "full" sql query
  val query: Query0[Vessel] = statement.query[Vessel] //.map{case (name, imo) => Vessel(name, imo)} //When .query bind result type and we get a query
  val lst: ConnectionIO[List[String]] = query.to[List] // specify what cardinality to expect of the result type, may now be composed with other queries inside a transaction
  val transaction: F[List[String]] = lst.transact(transactor) //takes the composed connectionIO(s) and creates a transaction boundary

 */
