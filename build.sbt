scalaVersion := "2.12.7"

organization := "org.srg"

name := "scala-xml-codec"

val testDependencies = Seq(
  //"org.typelevel" %% "scalaz-specs2" % "0.5.2",
  "org.specs2" %% "specs2-scalacheck" % "4.3.5",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.26-scalacheck-1.14"
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
