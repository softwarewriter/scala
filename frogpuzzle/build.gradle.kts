plugins {
    // Apply the java-library plugin to add support for Java Library
    scala
}

repositories {
    // Use jcenter for resolving dependencies.
    // You can declare any Maven/Ivy/file repository here.
    mavenCentral()
}

dependencies {
    compile("org.scala-lang:scala-library:2.13.3")
//    testCompile("org.scalatest:scalatest_2.11:2.2.2")
    testCompile("org.scalatest:scalatest_2.13:3.2.0")
}
