scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.8.0" % Test,
  "org.typelevel" %% "cats-core" % "2.0.0"
)