import mill._, scalalib._
object fsXtract extends RootModule with ScalaModule {
  def scalaVersion = "3.4.3"

  override
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.11.3",
    ivy"com.scalawilliam::xs4s-core:0.9.1",
    ivy"com.lihaoyi::mainargs:0.7.6",
    ivy"com.lihaoyi::pprint:0.9.0"
  )
}