name := "graphsc"

scalacOptions += "-deprecation"

mainClass in (Compile, run) := Some("graphsc.Test")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"
