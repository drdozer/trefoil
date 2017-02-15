resolvers += Resolver.sonatypeRepo("releases")

name := "trefoil"

organization := "uk.co.turingatemyhamster"

version := "0.1-SNAPSHOT"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "org.scorexfoundation" %% "scrypto" % "1.2.0"

scalaVersion := "2.12.1"

scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yliteral-types",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")