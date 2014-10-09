name := "nsi-scala"

version := "1.0-SNAPSHOT"

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "0.3.3",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test"
)

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

resolvers ++= Seq(
    "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty",
    "SURFnet BoD Snapshots" at s"$nexusBaseUri/public-snapshots",
    "SURFnet BoD Releases" at s"$nexusBaseUri/public-releases"
)

testFrameworks in Test := Seq(TestFrameworks.Specs2)

