import spray.revolver.RevolverPlugin.Revolver

name := "scala-cats-examples"

organization := "home"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

libraryDependencies ++= {
  val slf4jVersion = "1.7.12"
  val logbackVersion = "1.1.3"
  val catsVersion = "1.0.0-MF"
  Seq(
    //Logging
    "org.slf4j" % "slf4j-api" % slf4jVersion,
    "org.slf4j" % "log4j-over-slf4j" % slf4jVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "ch.qos.logback" % "logback-classic" % logbackVersion % "runtime",
    "ch.qos.logback" % "logback-core" % logbackVersion % "runtime",
    "com.typesafe" % "config" % "1.3.0",
    "org.typelevel" %% "cats-core" % catsVersion,
    // Test dependencies
    "org.scalatest" %% "scalatest" % "3.0.1"
  )
}

Revolver.settings

