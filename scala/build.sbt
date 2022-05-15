ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(name := "raytracer")

scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked",
    "-Xlint",
    "-feature",
    "-opt:l:method",
    "-opt-inline-from:**"
  )

fork := true

javaOptions := Seq("-Xms2G", "-Xmx2G", "-XX:+UseG1GC", "-XX:+UseStringDeduplication")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

enablePlugins(JmhPlugin)

addCommandAlias("bench", "jmh:run -i 3 -wi 3 -f1 -t1")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
