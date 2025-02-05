import sbt.Keys._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "List12",

    // https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.8.8"
  )


