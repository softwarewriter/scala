name := "list"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
   "org.typelevel"     %% "cats-effect"   % "2.2.0",
   "org.http4s"        %% "http4s-server" % "0.21.7",
   "org.http4s"        %% "http4s-blaze-server" % "0.21.7",
   "org.http4s"        %% "http4s-client" % "0.21.7",
   "org.http4s"        %% "http4s-dsl"    % "0.21.7",
   "org.scalatest" %% "scalatest" % "3.2.0" % Test
)