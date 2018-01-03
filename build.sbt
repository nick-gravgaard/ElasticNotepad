name := "Elastic Notepad"

version := "1.0.2"

scalaVersion := "2.12.4"

assemblyJarName in assembly := "ElasticNotepad.jar"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "2.0.0"
)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildInfo"
  )
