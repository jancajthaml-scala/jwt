import sbt._
import Keys._

name := "jwt"
//fork := true

version in Global := "0.1.0-SNAPSHOT"

description := "JSON Web Token issuer and validator"

scalaVersion in Global := "2.11.8"

/*
lazy val total_memory: String = (Process(new java.io.File("./get_memory.sh").getAbsolutePath) !!).split("[\\r\\n]+")(0)
*/

scalacOptions in Global ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint",
  //"-Xlint:deprecation",
  "-Yrangepos",
  "-language:postfixOps"
)

/*
javaOptions in Global ++= Seq(
  s"-Xmx$total_memory",
  s"-Xms$total_memory",
  "-XX:MaxMetaspaceSize={unlimited}",
  //"-Xmn1G",   //33% of XmS
  "-XX:SurvivorRatio=6",
  "-XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled"
)
*/

lazy val json = RootProject(uri("git://github.com/jancajthaml-scala/json.git#6c4b2c1db900a8f675625166290e5fbdb4a46d44"))
lazy val uuid = RootProject(uri("git://github.com/jancajthaml-scala/uuid.git#9701eca7123d551da4877fa550527ee685c965c5"))

lazy val test = Project(
  "test",
  file("."),
  settings = Defaults.coreDefaultSettings ++ Seq(
    publishArtifact := false,
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
      "Artima Maven Repository" at "http://repo.artima.com/releases"
    ),
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.8-SNAPSHOT",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    logBuffered := false
  )
).dependsOn(json).dependsOn(uuid)



