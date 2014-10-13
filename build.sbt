name := "nsi-scala"

version := "1.0-SNAPSHOT"

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "0.3.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "com.typesafe.play" %% "play" % "2.3.3",
  "com.typesafe.play" %% "play-test" % "2.3.3",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "org.specs2" %% "specs2-junit" % "2.3.13" % "test"
)

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

resolvers ++= Seq(
    "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty",
    "SURFnet BoD Snapshots" at s"$nexusBaseUri/public-snapshots",
    "SURFnet BoD Releases" at s"$nexusBaseUri/public-releases"
)

testFrameworks in Test := Seq(TestFrameworks.Specs2)

