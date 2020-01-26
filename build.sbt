name := "yalcr"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

scalacOptions ++= Seq(
  "-deprecation"
)
