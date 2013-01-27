import com.typesafe.sbt.SbtStartScript

name := "FacebookCup2013"

scalaVersion := "2.10.0"

scalacOptions += "-feature"

seq(SbtStartScript.startScriptForClassesSettings: _*)
