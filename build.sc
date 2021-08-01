import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo
import mill._
import scalalib._

object app extends ScalaModule with BuildInfo {
  def scalaVersion = "3.0.0"

  def appName = "Elastic Notepad"
  def appVersion = "1.6.1"
  val filename = "elastic-notepad.jar"

  override def buildInfoMembers: T[Map[String, String]] = T {
    Map(
      "appName" -> appName,
      "appVersion" -> appVersion
    )
  }

  override def unmanagedClasspath = T {
    if (!ammonite.ops.exists(millSourcePath / "lib")) Agg()
    else Agg.from(ammonite.ops.ls(millSourcePath / "lib").map(PathRef(_)))
  }

  override def assembly = T {
    val path = T.ctx().dest / filename
    os.move(super.assembly().path, path)
    PathRef(path)
  }

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-swing:3.0.0",
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.9",
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
