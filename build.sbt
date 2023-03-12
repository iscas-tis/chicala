ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.liuyic00"
ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "chicala",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.13.10"
    ),
    sbtPlugin := false
  )

val chiselVersion     = "3.5.6"
val chiselTestVersion = "0.5.6"
lazy val testcase = (project in file("testcase"))
  .settings(
    name := "testcase",
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3"    % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements"
    ),
    addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full),
    addCompilerPlugin("com.liuyic00"    %% "chicala"        % "0.1.0-SNAPSHOT")
  )
