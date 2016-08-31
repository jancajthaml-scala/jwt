import sbt.Keys._

name := "jwt"

version in Global := "0.1.0-SNAPSHOT"

description := "JSON Web Token issuer and validator"

scalaVersion in Global := "2.11.8"

scalacOptions in Global ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yrangepos",
  "-language:postfixOps")

lazy val jwt = project.in(file(".")).settings(
  name := "JWT test",
  mainClass in Compile := Some("com.github.jancajthaml.jwt.Main")
)
