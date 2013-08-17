import com.typesafe.sbt.SbtStartScript

scalaVersion := "2.10.1"

name := "graphsc"

scalacOptions += "-deprecation"

testOptions in Test += Tests.Argument("-P")

testOptions in Test += Tests.Argument("-oD")

mainClass in Compile := Some("graphsc.app.EqProverApp")

mainClass in oneJar := Some("graphsc.app.EqProverApp")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.2"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

seq(SbtStartScript.startScriptForClassesSettings: _*)

//seq(ScctPlugin.instrumentSettings : _*)
