name := "play-nsi-support"

organization := "nl.surfnet"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-adapted-args")

publishArtifact in Test := true

val playVersion = "2.3.6"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "0.3.6",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "com.typesafe.play" %% "play" % playVersion,
  "com.typesafe.play" %% "play-jdbc" % playVersion,
  "com.typesafe.play" %% "anorm" % playVersion,
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
  "com.typesafe.play" %% "play-test" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "org.specs2" %% "specs2-junit" % "2.3.13" % "test"
)

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"
val surfnetReleases = "SURFnet Releases" at s"$nexusBaseUri/public-releases"
val surfnetSnapshots = "SURFnet Snapshots" at s"$nexusBaseUri/public-snapshots"
val surfnetThirdParty = "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty"

resolvers ++= Seq( surfnetThirdParty, surfnetSnapshots, surfnetReleases )

publishTo := { if (isSnapshot.value) Some(surfnetSnapshots) else Some(surfnetReleases) }

testFrameworks in Test := Seq(TestFrameworks.Specs2)

releaseSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

lazy val licenseText = settingKey[String]("Project license text.")

licenseText := IO.read(baseDirectory.value / "LICENSE")

headers := Map(
  "scala" -> (
    HeaderPattern.cStyleBlockComment,
    licenseText.value.split("\n").map {
      case ""   => " *"
      case line => " * " ++ line
    }.mkString("/*\n", "\n", "\n */\n")
  )
)
