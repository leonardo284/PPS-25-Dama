ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "PPS-25-Dama",

    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "PPS-25-Dama.jar",

    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", _*) => MergeStrategy.discard
      case _                        => MergeStrategy.first
    },

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "junit" % "junit" % "4.13.2" % Test
    ),

    // Alias opzionale per lanciare tutto con un comando solo
    addCommandAlias("check", ";compile;test")
  )