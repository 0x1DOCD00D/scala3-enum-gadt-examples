ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala3-enum-gadt-examples",
    version := "0.1.0",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
  )
