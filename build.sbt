scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.12", scalaVersion.value)

updateOptions := updateOptions.value.withLatestSnapshots(false)

organization := "ch.srf"

name := "scala-xml-codec"

val testDependencies = Seq(
  "org.typelevel" %% "scalaz-specs2" % "0.5.2",
  "org.specs2" %% "specs2-scalacheck" % "4.0.2",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.26-scalacheck-1.13"
).map(_ % "test")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
  "org.scalaz" %% "scalaz-core" % "7.2.26"
) ++ testDependencies

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.NonUnitStatements)

wartremoverErrors in test -= Wart.NonUnitStatements

/* --------------------------------------------------------------------------------
 * Release
 * -------------------------------------------------------------------------------- */

releaseCrossBuild := true

releasePublishArtifactsAction := PgpKeys.publishSigned.value

releaseProcess := {
  import ReleaseTransformations._
  releaseProcess.value.dropRight(1) ++ Seq[ReleaseStep](
    releaseStepCommand("sonatypeRelease"),
    pushChanges
  )
}

/* --------------------------------------------------------------------------------
 * Publishing
 * -------------------------------------------------------------------------------- */

homepage := Some(url("https://github.com/mmz-srf/scala-xml-codec"))

scmInfo := Some(ScmInfo(
  url("https://github.com/mmz-srf/scala-xml-codec"),
  "git@github.com:username/projectname.git"))

developers := List(
  Developer(id = "devkat", name="Andreas Jim-Hartmann", email="andreas.hartmann@srf.ch", url = url("https://github.com/devkat"))
)

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

publishConfiguration := publishConfiguration.value.withOverwrite(true)

/* --------------------------------------------------------------------------------
 * GitHub pages
 * -------------------------------------------------------------------------------- */

enablePlugins(GhpagesPlugin, SiteScaladocPlugin)

git.remoteRepo := "git@github.com:{your username}/{your project}.git"

ghpagesNoJekyll := true
