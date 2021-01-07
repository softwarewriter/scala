organization := "jergan"
name := "scala"
version := "1.0.0"

scalaVersion := "2.13.3"

lazy val frogpuzzle = (project in file("frogpuzzle"))
lazy val list = (project in file("list"))
lazy val pellemacro = (project in file("macro"))
lazy val stack = (project in file("stack"))
lazy val scrapbook = (project in file("scrapbook"))
  .dependsOn(pellemacro)
