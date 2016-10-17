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

lazy val json = RootProject(uri("git://github.com/jancajthaml-scala/json.git#7e1458255940133bc7e760d72a4876ca37708cad"))
lazy val uuid = RootProject(uri("git://github.com/jancajthaml-scala/uuid.git#ba602be82868122493f7fdd347b5f796f9f353f6"))

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
      "com.storm-enroute" %% "scalameter" % "0.8.1",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    logBuffered := false
  )
).dependsOn(json).dependsOn(uuid)



