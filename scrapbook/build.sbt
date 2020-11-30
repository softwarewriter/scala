name := "scrapbook"

scalaVersion := "2.13.3"


scalacOptions := Seq(
   "-language:higherKinds",
)

libraryDependencies ++= Seq(
   "org.typelevel"     %% "cats-effect"   % "2.2.0",

   "org.http4s"        %% "http4s-server" % "0.21.7",
   "org.http4s"        %% "http4s-blaze-server" % "0.21.7",
   "org.http4s"        %% "http4s-client" % "0.21.7",
   "org.http4s"        %% "http4s-blaze-client" % "0.21.7",
   "org.http4s"        %% "http4s-dsl"    % "0.21.7",
   "org.http4s"        %% "http4s-circe"    % "0.21.7",

   "io.circe"          %% "circe-core" % "0.12.3",

   "org.apache.kafka" % "kafka-streams" % "0.11.0.0")
