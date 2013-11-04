name := "Sep"

version := "0.1.0"

scalaVersion := "2.10.2"

organization := "com.github.bmatsuo"

resolvers ++= Seq(
        "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
        "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
        "com.github.tototoshi" %% "scala-csv" % "1.0.0-SNAPSHOT",
        "org.scalaz"     %% "scalaz-core"      % "7.0.3",
        "org.parboiled"  %% "parboiled-scala"  % "1.1.6",
        "org.scalacheck" %% "scalacheck"       % "1.11.0" % "test")

javaOptions in test += "-Xmx512m"
