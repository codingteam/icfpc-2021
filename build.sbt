name := "icfpc-2021"

version := "0.1"

scalaVersion := "2.13.6"

scalacOptions += "-deprecation"

scalacOptions += "-optimize"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.4"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.12.4"
