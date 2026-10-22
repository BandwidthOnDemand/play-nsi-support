name := "play-nsi-support"

organization := "nl.surfnet"

Global / onChangedBuildSource := ReloadOnSourceChanges

scalaVersion := "3.3.8"

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

val playVersion = "3.0.12"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "3.0.1",
  "org.glassfish.jaxb" % "jaxb-runtime" % "4.0.9",
  "org.scala-lang.modules" %% "scala-xml" % "2.5.0",
  "org.playframework" %% "play" % playVersion,
  "org.playframework" %% "play-jdbc" % playVersion,
  "org.playframework" %% "play-json" % "3.0.6",
  "org.playframework.anorm" %% "anorm" % "3.1.0",
  "org.playframework" %% "play-specs2" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.23.0" % "test",
  "org.postgresql" % "postgresql" % "42.7.14" % "test",
  // plexus-utils 4 moved its xml classes here; maven-model under play-test still needs them.
  "org.codehaus.plexus" % "plexus-xml" % "3.1.0" % "test"
)

// play-test's unused WithBrowser helper pulls Appium through FluentLenium.
ThisBuild / excludeDependencies += ExclusionRule("io.appium")

dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-core" % "2.22.3",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.22.3",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.22",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % "2.22.3",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.22.3",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.22.3.1",
  "commons-beanutils" % "commons-beanutils" % "1.11.0",
  "org.apache.commons" % "commons-lang3" % "3.21.0",
  "org.codehaus.plexus" % "plexus-utils" % "4.1.0",
  "ch.qos.logback" % "logback-core" % "1.6.5",
  "ch.qos.logback" % "logback-classic" % "1.6.5",
  "at.yawk.lz4" % "lz4-java" % "1.12.0",
  "org.jsoup" % "jsoup" % "1.23.2",
  // Scala 3 compiler/scaladoc toolchain only (scalaInstance), never on the app classpath.
  "org.jline" % "jline-reader" % "3.30.17",
  "org.jline" % "jline-terminal" % "3.30.17",
  "org.jline" % "jline-terminal-jni" % "3.30.17",
  "org.jline" % "jline-native" % "3.30.17",
  // Selenium/HtmlUnit test-only deps (via play-test's WithBrowser helper, unused by this
  // library): opentelemetry-api and jetty-http can move within their own line without
  // disturbing the selenium-api version htmlunit-driver 4.13.0 was built against.
  "io.opentelemetry" % "opentelemetry-api" % "1.67.0",
  "org.eclipse.jetty" % "jetty-http" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-client" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-io" % "9.4.58.v20250814",
  "org.eclipse.jetty" % "jetty-util" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-api" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-client" % "9.4.58.v20250814",
  "org.eclipse.jetty.websocket" % "websocket-common" % "9.4.58.v20250814"
)

resolvers += Resolver.typesafeRepo("releases")

githubOwner := "BandwidthOnDemand"
githubRepository := "play-nsi-support"
resolvers += Resolver.githubPackages("BandwidthOnDemand")

Test / testFrameworks := Seq(TestFrameworks.Specs2)

organizationName := "SURFnet B.V."
startYear := Some(2012)
licenses += ("BSD-3-Clause", new URL("file:LICENSE"))
