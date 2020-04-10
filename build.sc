//import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:0.5.0`
import mill.contrib.buildinfo.BuildInfo
import mill._, scalalib._

object app extends ScalaModule with BuildInfo {
  def scalaVersion = "2.13.0"

  def appName = "Elastic Notepad"
  def appVersion = "1.4.0"
  val filename = "elastic-notepad.jar"

  def buildInfoMembers: T[Map[String, String]] = T {
    Map(
      "appName" -> appName,
      "appVersion" -> appVersion
    )
  }

  def unmanagedClasspath = T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }

  def assembly = T {
    val path = T.ctx().dest / filename
    os.move(super.assembly().path, path)
    PathRef(path)
  }

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-swing:2.1.1",
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.1.1",
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
