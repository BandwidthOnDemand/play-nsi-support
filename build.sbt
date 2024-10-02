name := "play-nsi-support"

organization := "nl.surfnet"

scalaVersion := "2.13.14"

scalacOptions ++= Seq(
  "-Xsource:3",
  "-Xsource-features:case-apply-copy-access",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-release:21"
)

Test / publishArtifact := true

// Disable ScalaDoc generation
Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false

Test / doc / sources := Seq.empty
Test / packageDoc / publishArtifact := false

val playVersion = "2.9.4"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "3.0.0-SNAPSHOT",
  "org.glassfish.jaxb" % "jaxb-runtime" % "4.0.5",
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "com.typesafe.play" %% "play" % playVersion,
  "com.typesafe.play" %% "play-jdbc" % playVersion,
  "com.typesafe.play" %% "play-json" % playVersion,
  "org.playframework.anorm" %% "anorm" % "2.7.0",
  "com.typesafe.play" %% "play-specs2" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.20.7" % "test"
)

resolvers += Resolver.typesafeRepo("releases")

githubOwner := "BandwidthOnDemand"
githubRepository := "play-nsi-support"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

Test / scalacOptions ++= Seq("-Yrangepos")

Test / testFrameworks := Seq(TestFrameworks.Specs2)

organizationName := "SURFnet B.V."
startYear := Some(2012)
licenses += ("BSD-3-Clause", new URL("file:LICENSE"))
