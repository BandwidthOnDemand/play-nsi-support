name := "play-nsi-support"

organization := "nl.surfnet"

Global / onChangedBuildSource := ReloadOnSourceChanges

scalaVersion := "3.3.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports,privates,locals,params",
  "-release:21"
)

Test / publishArtifact := true

// Disable ScalaDoc generation
Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false

Test / doc / sources := Seq.empty
Test / packageDoc / publishArtifact := false

val playVersion = "3.0.5"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "3.0.0",
  "org.glassfish.jaxb" % "jaxb-runtime" % "4.0.5",
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "org.playframework" %% "play" % playVersion,
  "org.playframework" %% "play-jdbc" % playVersion,
  "org.playframework" %% "play-json" % "3.0.4",
  "org.playframework.anorm" %% "anorm" % "2.7.0",
  "org.playframework" %% "play-specs2" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.20.7" % "test"
)

resolvers += Resolver.typesafeRepo("releases")

githubOwner := "BandwidthOnDemand"
githubRepository := "play-nsi-support"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

Test / testFrameworks := Seq(TestFrameworks.Specs2)

organizationName := "SURFnet B.V."
startYear := Some(2012)
licenses += ("BSD-3-Clause", new URL("file:LICENSE"))
