name := "jwt"

version := "0.1.0-SNAPSHOT"

description := "JSON Web Token issuer and validator"

scalaVersion := "2.11.8"

lazy val demo = (project in file(".")).settings(
  name := "JWT test",
  mainClass in Compile := Some("com.github.jancajthaml.jwt.Main")
)