import com.typesafe.sbt.SbtStartScript

name := "FacebookCup2013"

scalaVersion := "2.10.0"

scalacOptions += "-feature"

scalacOptions += "-optimize"

seq(SbtStartScript.startScriptForClassesSettings: _*)

mainClass in Compile := Some("FindTheMin")

libraryDependencies += "com.codecommit" %% "gll-combinators" % "2.2-SNAPSHOT"
