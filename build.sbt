
organization := "com.github.ellbur"

name := "redo-signals"

version := "0.3-SNAPSHOT"

scalaVersion := "2.11.0-M6"

scalaSource in Compile <<= baseDirectory(_ / "src")

// https://github.com/harrah/xsbt/wiki/Publishing
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "cc.co.scala-reactive" % "reactive-core_2.11.0-M6" % "0.3.0"
)

