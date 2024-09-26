// Comment to get more information during initialization
logLevel := Level.Warn

addDependencyTreePlugin

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")

addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")

addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")
