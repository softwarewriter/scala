package vesselapi

import cats.data.NonEmptyList
import cats.effect.{Blocker, IO, Resource}
import doobie.scalatest.IOChecker
import org.scalatest.matchers.should.Matchers
import platform.test.SharedResourceSpec
import vesselapi.VesselRepository.QueryFilters
import vesselapi.model.{DataloyVessel, Imo, TestJson}

class DatabaseRepositorySpec extends SharedResourceSpec {
  type FixtureParam = IOChecker with Matchers

  val dbPort: Int = platform.test.availablePort

  val resource: Resource[IO, FixtureParam] = for {
    dbCreds        <- platform.test.Database[IO](port = dbPort, databaseName = "doobie_db_checker", schemaName = Some("vessel_api"))
    blocker        <- Blocker[IO]
    testTransactor <- platform.Database.transactor[IO](dbCreds, blocker)
  } yield
    new IOChecker with Matchers {
      override def transactor: doobie.Transactor[IO] = testTransactor
    }

  "Validate Database Queries" - {
    val vessel = TestJson.decodeClarksonsVesselSchema().toOption.get
    val consumptions = TestJson
      .decodeDataloyVesselSchema()
      .flatMap(v => DataloyVessel.dataloyVesselConverter.convert(v))
      .toOption
      .get
      .map(c => c.copy(imo = Imo(vessel.imo)))

    "VesselRepository" - {

      "Lookup vessel IMO" in { checker =>
        IO(checker.check(VesselRepository.Q.exists("1234567")))
      }
      "Insert" in { checker =>
        IO(checker.check(VesselRepository.Q.insert(vessel)))
      }
      "Update" in { checker =>
        IO(checker.check(VesselRepository.Q.update(vessel)))
      }
      "Search for IMO" in { checker =>
        IO(checker.check(VesselRepository.Q.list(QueryFilters(Nil, List("9806952")))))
      }
      "Search for name" in { checker =>
        IO(checker.check(VesselRepository.Q.list(QueryFilters(List("Que"), Nil))))
      }
      "Search for IMO and name" in { checker =>
        IO(checker.check(VesselRepository.Q.list(QueryFilters(List("Que"), List("9806952")))))
      }
      "Call search with no filters" in { checker =>
        IO(checker.check(VesselRepository.Q.list(QueryFilters(Nil, Nil))))
      }
    }

    "ConsumptionRepository" - {
      "Insert" in { checker =>
        IO(checker.check(ConsumptionRepository.Q.insert))
      }

      "Delete" in { checker =>
        IO(checker.check(ConsumptionRepository.Q.delete("1234567")))
      }

      "Delete NEL" in { checker =>
        IO(checker.check(ConsumptionRepository.Q.delete(NonEmptyList.of("1234567", "456789"))))
      }
      "Lookup by imo" in { checker =>
        IO(checker.check(ConsumptionRepository.Q.get("123456789")))
      }
    }

    "Round Trip" in { checker =>
      import doobie.implicits._

      val db = for {
        _     <- VesselRepository.Q.insert(vessel).run
        _     <- ConsumptionRepository.upsert(consumptions)
        found <- VesselRepository.get(vessel.imo)
      } yield found
      db.transact(checker.transactor)
        .map { _.map(consumptions => assert(consumptions === consumptions)).getOrElse(fail()) }
    }
  }
}
