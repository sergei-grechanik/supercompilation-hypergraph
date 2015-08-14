import com.typesafe.sbt.SbtStartScript

scalaVersion := "2.10.1"

name := "graphsc"

scalacOptions += "-deprecation"

testOptions in Test += Tests.Argument("-P")

testOptions in Test += Tests.Argument("-oD")

mainClass in Compile := Some("graphsc.app.MainApp")

//mainClass in oneJar := Some("graphsc.app.MainApp")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.2"

//libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.0.0.1"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1"

libraryDependencies += "net.sf.jung" % "jung-visualization" % "2.0.1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.1"

//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

seq(SbtStartScript.startScriptForClassesSettings: _*)

//seq(ScctPlugin.instrumentSettings : _*)
