name := "play-nsi-support"

organization := "nl.surfnet"

scalaVersion := "2.13.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-Ywarn-unused", "-Ywarn-value-discard", "-target:jvm-1.8")

Test / publishArtifact := true

// Disable ScalaDoc generation
Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false

Test / doc / sources := Seq.empty
Test / packageDoc / publishArtifact := false

val playVersion = "2.7.9"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "2.1.5",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "com.typesafe.play" %% "play" % playVersion,
  "com.typesafe.play" %% "play-jdbc" % playVersion,
  "com.typesafe.play" %% "play-json" % "2.7.4",
  "org.playframework.anorm" %% "anorm" % "2.6.10",
  "com.typesafe.play" %% "play-test" % playVersion % "test",
  "com.typesafe.play" %% "play-specs2" % playVersion % "test",
  //"com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3-1",
  "org.specs2" %% "specs2-scalacheck" % "4.13.0" % "test"
)

// val githubBodNsi = "https://maven.pkg.github.com/BandwidthOnDemand/bod-nsi"
// val githubPlayNsiSupport = "BandwidthOnDemand play-nsi-support" at s"https://maven.pkg.github.com/BandwidthOnDemand/play-nsi-support"
// val surfnetReleases = "SURFnet Releases" at s"$nexusBaseUri/public-releases"
// val surfnetSnapshots = "SURFnet Snapshots" at s"$nexusBaseUri/public-snapshots"
// val surfnetThirdParty = "SURFnet thirdparty" at s"$nexusBaseUri/thirdparty"

resolvers += Resolver.typesafeRepo("releases")

githubOwner := "BandwidthOnDemand"
githubRepository := "play-nsi-support"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

Test / scalacOptions ++= Seq("-Yrangepos")

// publishTo := githubPlayNsiSupport

Test / testFrameworks := Seq(TestFrameworks.Specs2)

//net.virtualvoid.sbt.graph.Plugin.graphSettings

lazy val licenseText = settingKey[String]("Project license text.")

licenseText := IO.read(baseDirectory.value / "LICENSE")

// headers := Map(
//   "scala" -> (
//     HeaderPattern.cStyleBlockComment,
//     licenseText.value.split("\n").map {
//       case ""   => " *"
//       case line => " * " ++ line
//     }.mkString("/*\n", "\n", "\n */\n")
//   )
// )
