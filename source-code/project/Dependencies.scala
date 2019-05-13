import sbt._

object Dependencies {
  private val scalatestVersion = "3.0.5"
  private val scalacheckVersion = "1.14.0"
  private val spireVersion = "0.16.0"
  private val catsVersion = "1.5.0"
  private val catsEffectVersion = "1.2.0"
  private val disciplineVersion = "0.11.0"
  private val catsLawsVersion = "1.6.0"
  private val akkaVersion = "2.5.21"
  private val mouseVersion = "0.20"
  private val fs2Version = "1.0.4"

  lazy val spire = "org.typelevel" %% "spire" % spireVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
  lazy val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  lazy val mouse = "org.typelevel" %% "mouse" % mouseVersion
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  lazy val fs2 = "co.fs2" %% "fs2-core" % fs2Version

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion
  lazy val discipline = "org.typelevel" %% "discipline" % disciplineVersion
  lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsLawsVersion

  lazy val libs: Vector[ModuleID] =
    Vector(spire, cats, akkaStream, mouse, catsEffect, fs2)

  lazy val testLibs: Vector[ModuleID] =
    Vector(scalaTest, scalaCheck, discipline, catsLaws).map(_ % Test)
}
