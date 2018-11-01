scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.12", scalaVersion.value)

organization := "ch.srf"

name := "scala-xml-codec"

val testDependencies = Seq(
  "org.typelevel" %% "scalaz-specs2" % "0.5.2",
  "org.specs2" %% "specs2-scalacheck" % "4.0.2",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.26-scalacheck-1.13"
).map(_ % "test")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
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

releaseProcess := {
  import ReleaseTransformations._
  Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommand("publishSigned"),
    releaseStepCommand("sonatypeRelease"),
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
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

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

