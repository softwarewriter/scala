organization := "jergan"
name := "scala"
version := "1.0.0"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

lazy val frogpuzzle = (project in file("frogpuzzle"))
lazy val list = (project in file("list"))
