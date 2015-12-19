
name := "parse-trees"

organization := "iguana"

version := "0.1.0"

isSnapshot := true 

scalaVersion := "2.11.7"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "junit" % "junit" % "4.11",
  "commons-cli" % "commons-cli" % "1.2"
)

lazy val utils = ProjectRef(file("../utils"), "utils")

val main = Project(id = "parse-trees", base = file(".")).dependsOn(utils)
