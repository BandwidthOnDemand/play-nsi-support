name := "play-nsi-support"

organization := "nl.surfnet"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-adapted-args")

publishArtifact in Test := true

// Disable ScalaDoc generation
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false

sources in (Test, doc) := Seq.empty
publishArtifact in (Test, packageDoc) := false

val playVersion = "2.3.10"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "2.1.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "com.typesafe.play" %% "play" % playVersion,
  "com.typesafe.play" %% "play-jdbc" % playVersion,
  "com.typesafe.play" %% "anorm" % playVersion,
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
  "com.google.code.findbugs" % "jsr305" % "3.0.0", // for javax.annotation.Nullable
  "com.typesafe.play" %% "play-test" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.3.13" % "test",
  "org.specs2" %% "specs2-junit" % "2.3.13" % "test"
)

val nexusBaseUri = "https://atlas.dlp.surfnet.nl/nexus/content/repositories"
val surfnetReleases = "SURFnet Releases" at s"$nexusBaseUri/public-releases"
val surfnetSnapshots = "SURFnet Snapshots" at s"$nexusBaseUri/public-snapshots"
val surfnetThirdParty = "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty"
val scalazReleases = "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

resolvers += Resolver.typesafeRepo("releases")

resolvers ++= Seq( surfnetThirdParty, surfnetSnapshots, surfnetReleases, scalazReleases )

scalacOptions in Test ++= Seq("-Yrangepos")

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
