ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(name := "raytracer")

fork := true

javaOptions := Seq("-Xms2G", "-Xmx2G", "-XX:+UseG1GC", "-XX:+UseStringDeduplication")

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

enablePlugins(JmhPlugin)

addCommandAlias("bench", "jmh:run -i 3 -wi 3 -f1 -t1")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
