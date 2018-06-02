name := "mongo-macros"

version := "0.1"

scalaVersion := "2.12.6"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.3.0"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.0.2" % "test"