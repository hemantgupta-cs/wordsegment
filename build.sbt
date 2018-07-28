scalaVersion := "2.11.11"

name := "wordsegment"
version := "0.0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

fork in Test := true
javaOptions ++= Seq("-Xms512M", "-Xmx2048M")
parallelExecution in Test := false
