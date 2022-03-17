scalaVersion := "2.13.6"

scalacOptions --= List("-Xlint:inaccessible")

ThisBuild / versionScheme := Some("early-semver")

updateOptions := updateOptions.value.withLatestSnapshots(false)

organization := "ch.srf"

name := "scala-xml-codec"

val testDependencies = Seq(
  "org.specs2" %% "specs2-scalacheck" % "4.12.3",
  "io.chrisdavenport" %% "cats-scalacheck" % "0.2.0",
  "org.typelevel" %% "cats-laws" % "2.6.1",
).map(_ % "test")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scala-lang.modules" %% "scala-xml" % "2.0.0",
  "org.typelevel" %% "cats-core" % "2.6.1"
) ++ testDependencies ++ List(compilerPlugin("org.typelevel" % ("kind-projector_" + scalaVersion.value) % "0.13.0"))

wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.NonUnitStatements)

Test / wartremoverErrors -= Wart.NonUnitStatements

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
