
organization := "com.github.ellbur"

name := "redo-signals"

version := "0.8-SNAPSHOT"

scalaVersion := "2.11.4"

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

javaSource in Test <<= baseDirectory(_ / "test")

resourceDirectory in Compile <<= baseDirectory(_ / "resources")

resourceDirectory in Test <<= baseDirectory(_ / "test-resources")

// https://github.com/harrah/xsbt/wiki/Publishing
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "cc.co.scala-reactive" % "reactive-core_2.11.0-M6" % "0.3.0",
  "org.scala-lang" % "scala-actors" % "2.11.0",
  "com.github.ellbur" % "dependent-map_2.11" % "2.0-SNAPSHOT"
)

//libraryDependencies ++= Seq(
//  "com.intellij" % "forms_rt" % "7.0.3" % "test"
//)

resolvers += "Local Maven Repository" at file(Path.userHome.absolutePath + "/.m2/repository").toURL.toString

