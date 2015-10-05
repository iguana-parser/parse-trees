
name := "parse-trees"

organization := "iguana"

version := "0.1.0"

isSnapshot := true 

scalaVersion := "2.11.7"

parallelExecution in Test := false

val localMaven = "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += localMaven

publishTo := Some(localMaven)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
	"junit" % "junit" % "4.11",
  "iguana" % "utils" % "0.1.0"
)

addCommandAlias("generate-project", ";update-classifiers;update-sbt-classifiers;gen-idea sbt-classifiers")

// SBT Eclipse configuration

//EclipseKeys.eclipseOutput in Compile := Some("bin/main/scala")
//EclipseKeys.eclipseOutput in Test := Some("bin/test/scala")

//EclipseKeys.withSource := true

