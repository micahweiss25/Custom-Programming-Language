lazy val root = project.in(file(".")).settings(
  name := "homeworks",
  scalaVersion := "3.0.0-RC3",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
  ),
  logLevel := Level.Warn,
  maxErrors := 10, // maximum number of errors shown by the Scala compiler
  resolvers += "EECS Repo" at "https://eecscourses.westpoint.edu/courses/cs478/scala/releases",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.8",
  libraryDependencies += "eecs" %% "eecstester" % "0.7.0delta4",
  libraryDependencies += "eecs" %% "regchecker" % "0.1.1",
)
