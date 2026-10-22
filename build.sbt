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

val playVersion = "3.0.10"

libraryDependencies ++= Seq(
  "nl.surfnet.bod" % "bod-nsi" % "3.0.0",
  "org.glassfish.jaxb" % "jaxb-runtime" % "4.0.5",
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "org.playframework" %% "play" % playVersion,
  "org.playframework" %% "play-jdbc" % playVersion,
  "org.playframework" %% "play-json" % "3.0.4",
  "org.playframework.anorm" %% "anorm" % "3.1.0",
  "org.playframework" %% "play-specs2" % playVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.20.7" % "test",
  "org.postgresql" % "postgresql" % "42.7.12" % "test",
  // plexus-utils 4 moved its xml classes here; maven-model under play-test still needs them.
  "org.codehaus.plexus" % "plexus-xml" % "3.1.0" % "test"
)

// play-test's unused WithBrowser helper pulls Appium through FluentLenium.
ThisBuild / excludeDependencies += ExclusionRule("io.appium")

dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-core" % "2.18.11",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.18.11",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.18.11",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % "2.18.11",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % "2.18.11",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.11",
  "commons-beanutils" % "commons-beanutils" % "1.11.0",
  "org.apache.commons" % "commons-lang3" % "3.18.0",
  "org.codehaus.plexus" % "plexus-utils" % "4.1.0",
  "ch.qos.logback" % "logback-core" % "1.5.34",
  "ch.qos.logback" % "logback-classic" % "1.5.34",
  "at.yawk.lz4" % "lz4-java" % "1.11.4",
  "org.jsoup" % "jsoup" % "1.23.2",
  // Scala 3 compiler/scaladoc toolchain only (scalaInstance), never on the app classpath.
  "org.jline" % "jline-reader" % "3.30.15",
  "org.jline" % "jline-terminal" % "3.30.15",
  "org.jline" % "jline-terminal-jni" % "3.30.15",
  "org.jline" % "jline-native" % "3.30.15",
  // Selenium/HtmlUnit test-only deps (via play-test's WithBrowser helper, unused by this
  // library): opentelemetry-api and jetty-http can move within their own line without
  // disturbing the selenium-api version htmlunit-driver 4.13.0 was built against.
  "io.opentelemetry" % "opentelemetry-api" % "1.62.0",
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
