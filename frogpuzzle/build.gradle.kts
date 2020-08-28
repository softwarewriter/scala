plugins {
    scala
    id("com.github.maiflai.scalatest") version "0.26"
}

repositories {
    mavenCentral()
}

dependencies {
    compile("org.scala-lang:scala-library:2.13.3")
    testCompile("org.scalatest:scalatest_2.13:3.2.0")
    testRuntime("com.vladsch.flexmark:flexmark-all:0.35.10")
}
