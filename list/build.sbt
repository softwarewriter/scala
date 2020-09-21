name := "list"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4" withSources() withJavadoc()

libraryDependencies += "com.pepegar" %% "hammock-core" % "0.10.0"