ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises",
    libraryDependencies += "org.wvlet.airframe" %% "airspec" % "21.12.1" % Test,
    testFrameworks += new TestFramework("wvlet.airspec.Framework"),
    Test / parallelExecution := false
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )
