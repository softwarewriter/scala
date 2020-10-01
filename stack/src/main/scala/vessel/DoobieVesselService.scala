package vessel

import cats.effect.Bracket
import cats.free.Free
import cats.implicits._
import doobie._
import doobie.`enum`.JdbcType
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
   implicit val IMOMeta: Meta[IMO] = //eq json Codec
      Meta[String].timap(s => IMO(s))(imo => imo.value)

   */

/*
   implicit val imoReader: Get[IMO] = Get[String].map(IMO) // samme som decoder
   implicit val imoWriter: Put[IMO] = Put[String].contramap(_.value) //samme som encoder

 */

  /*
   implicit val JsonMeta: Meta[Json] =
      Meta.other[PGobject]("json").nxmap[Json](
         a => Parse.parse(a.getValue).leftMap[Json](sys.error).merge, // failure raises an exception
         a => new PGobject <| (_.setType("json")) <| (_.setValue(a.nospaces))
      )


   )
   */


   override def get(imo: IMO): F[Option[Vessel]] = {
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

   override def delete(imo: IMO): F[Option[Vessel]] = {
      val v1: ConnectionIO[Option[Vessel]] = Connection.get(imo).flatMap{
         case Some(vessel) =>
            Connection.delete(imo).map(_ => Option(vessel))
         case None =>
            "no-operation".pure[ConnectionIO].map(_ => None)
      }
      v1.transact(transactor)
   }

   override def search(queryString: String): F[List[Vessel]] = {
      Q.search(queryString)
         .to[List]
         .transact(transactor)
   }

  object Connection {

     def get(imo: IMO): ConnectionIO[Option[Vessel]] = {
        Q.get(imo)
           .option
     }

     def insert(vessel: Vessel): ConnectionIO[Int] = {
       Q.insert(vessel)
           .run
     }

     def delete(imo: IMO): ConnectionIO[Int] = {
       Q.delete(imo)
           .run
     }

  }


  // DoobieChecker i Vessel api.

  // case class Vessel(imo: IMO, name: String) //case class
  //                  (imo: IMO, name: String) //Tuple(IMO, String)

  object Q {

     def get(imo: IMO): Query0[Vessel] = {
        sql"""select imo, name from vessel where imo = $imo"""
           .query[Vessel]
     }

     def insert(vessel: Vessel): Update0 = {
        sql"""insert into vessel (imo, name) values (${vessel.imo}, ${vessel.name})"""
           .update
     }

     def delete(imo: IMO): Update0 = {
        sql"""delete from vessel where imo = $imo"""
           .update
     }

     def search(queryString: String): Query0[Vessel] = {
        val queryStringWithWildcards = s"%$queryString%"
        sql"""select imo, name from vessel where imo like $queryStringWithWildcards or name like $queryStringWithWildcards"""
              .query[Vessel]
     }

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
