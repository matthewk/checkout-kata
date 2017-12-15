import sbt.Keys.scalacOptions

name := "checkout-kata"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xmax-classfile-name","100"
)

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"    % "3.0.4"       % "test",
  "org.typelevel"     %% "cats-core"    % "1.0.0-RC1"
)