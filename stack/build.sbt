enablePlugins(JavaAppPackaging)

organization := "com.klavenessdigital"

name := "vessel-poc"

version := "1.0"

scalaVersion := "2.13.3"

resolvers ++= Seq(
  "klab.jfrog.io-latest-builds" at "https://klab.jfrog.io/klab/list/latest-builds"
)

credentials ++= Seq(
  Credentials(Path.userHome / ".sbt" / ".credentials")
)

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Xcheckinit"
)

libraryDependencies ++= Seq(
  "com.klavenessdigital" %% "platform-sdk"  % "15",
  "org.scalatest"        %% "scalatest"     % "3.2.0" % "test",
  "io.circe"             %% "circe-literal" % "0.13.0" % "test"
)