name := "play-nsi-support"

organization := "nl.surfnet"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

publishArtifact in Test := true

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "0.3.6",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "com.typesafe.play" %% "play" % "2.3.3",
  "com.typesafe.play" %% "play-jdbc" % "2.3.3",
  "com.typesafe.play" %% "anorm" % "2.3.3",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
  "com.typesafe.play" %% "play-test" % "2.3.3" % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "org.specs2" %% "specs2-junit" % "2.3.13" % "test"
)

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"
val surfnetReleases = "SURFnet Releases" at s"$nexusBaseUri/public-releases"
val surfnetSnapshots = "SURFnet Snapshots" at s"$nexusBaseUri/public-snapshots"
val surfnetThirdParty = "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty"

resolvers ++= Seq( surfnetThirdParty, surfnetSnapshots, surfnetReleases )

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

publishTo := { if (isSnapshot.value) Some(surfnetSnapshots) else Some(surfnetReleases) }

testFrameworks in Test := Seq(TestFrameworks.Specs2)

releaseSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings
