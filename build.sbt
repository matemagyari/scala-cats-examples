name := "scala-cats-examples"

organization := "home"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

libraryDependencies ++= {
  val catsVersion = "1.6.0"
  Seq(
    "com.typesafe" % "config" % "1.3.0",
    "org.typelevel" %% "cats-core" % catsVersion,
    // Test dependencies
    "org.scalatest" %% "scalatest" % "3.0.1"
  )
}

scalacOptions += "-Ypartial-unification"