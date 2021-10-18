name := "scrapbook"

scalaVersion := "2.13.3"

scalacOptions := Seq(
   "-language:higherKinds",
   "-language:implicitConversions"
)

libraryDependencies ++= Seq(
   "ch.qos.logback"   %  "logback-classic" % "1.2.0",

   "org.typelevel"     %% "cats-core"   % "2.2.0",
   "org.typelevel"     %% "cats-effect"   % "2.2.0",

   "org.http4s"        %% "http4s-server" % "0.21.7",
   "org.http4s"        %% "http4s-blaze-server" % "0.21.7",
   "org.http4s"        %% "http4s-client" % "0.21.7",
   "org.http4s"        %% "http4s-blaze-client" % "0.21.7",
   "org.http4s"        %% "http4s-dsl"    % "0.21.7",
   "org.http4s"        %% "http4s-circe"    % "0.21.7",
   "com.chuusai"        %% "shapeless"           % "2.3.3",

   "io.circe"          %% "circe-generic"    % "0.13.0",
   "io.circe"          %% "circe-parser"    % "0.13.0",
   "io.circe"          %% "circe-fs2"       % "0.13.0",
   "co.fs2"            %% "fs2-io"          % "2.4.4",

   "org.apache.kafka" % "kafka-streams"    % "0.11.0.0",
   "org.scalatest"    %% "scalatest"           % "3.2.9"

)


