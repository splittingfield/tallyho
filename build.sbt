name := "tallyho"

organization := "io.abacus"

version := "0.0.1"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  "socrata maven" at "https://repository-socrata-oss.forge.cloudbees.com/release",
     "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
       "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "socrata maven-snap" at "https://repository-socrata-oss.forge.cloudbees.com/snapshot",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  Classpaths.sbtPluginReleases
)

libraryDependencies ++= Seq(
  "com.clearspring.analytics" % "stream"% "2.7.0", 
    "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
"org.scalatest" % "scalatest_2.10" % "2.2.0" % "test" withSources() withJavadoc(),
"org.scalacheck" %% "scalacheck" % "1.10.0" % "test" withSources() withJavadoc(),
"org.apache.commons" % "commons-lang3" % "3.3.2",
"com.typesafe" % "config" % "1.0.2",
"com.twitter" % "algebird_2.10" % "0.7.0" withSources() withJavadoc(),
"com.twitter" % "algebird-core_2.10" % "0.7.0" withSources() withJavadoc(),
"com.twitter" % "algebird-util_2.10" % "0.7.0" withSources() withJavadoc(),
 "com.github.stephenc.high-scale-lib" % "high-scale-lib" % "1.1.4"
)



testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

// WARNING: -optimize is not recommended with akka, should that come up.
// NOTE: Having to remove -Xfatal-warnings because it chokes due to inliner issues.
// This really bothers me.
scalacOptions ++= Seq("-optimize", "-deprecation", "-feature","-language:postfixOps", "-Xlint")




