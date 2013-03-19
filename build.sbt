import com.typesafe.sbt.SbtStartScript

name := "graphsc"

scalacOptions += "-deprecation"

mainClass in Compile := Some("graphsc.app.EqProverApp")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "org.rogach" %% "scallop" % "0.8.1"

seq(SbtStartScript.startScriptForClassesSettings: _*)

//seq(ScctPlugin.instrumentSettings : _*)
