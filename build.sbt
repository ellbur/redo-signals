
organization := "com.github.ellbur"

name := "redo-signals"

version := "0.2-SNAPSHOT"

scalaVersion := "2.10.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

// https://github.com/harrah/xsbt/wiki/Publishing
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "cc.co.scala-reactive" %% "reactive-core" % "0.3.0"
)


