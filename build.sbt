import sbt._
import sbt.Keys._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val circeVersion = "0.14.7"

scalaVersion := "2.13.13"

lazy val assembly =
  crossProject(JSPlatform, JVMPlatform)
    .settings(
        name := "assembly",

        scalaVersion := "2.13.13",

        organization := "com.github.antonkulaga",

	      version := "0.0.15",

        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

        exportJars := true,

        run / fork := true,

        parallelExecution in Test := false,

        licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),

        resolvers += "jitpack.io" at "https://jitpack.io",

        libraryDependencies ++= Seq(
            "io.circe" %%% "circe-core" % circeVersion,
            "io.circe" %%% "circe-generic" % circeVersion,
            "io.circe" %%% "circe-generic-extras" % "0.14.3",//circeVersion,
            "io.circe" %%% "circe-parser" % circeVersion,
            "org.scalatest" %%% "scalatest" % "3.1.4" % Test
        ),

      scalacOptions ++= Seq("-feature", "-language:_", "-Ymacro-annotations"),

      Seq(Compile, Test, Runtime).flatMap(inConfig(_) {
        unmanagedResourceDirectories ++= {
          unmanagedSourceDirectories.value
            .map(src => (src / ".." / "resources").getCanonicalFile)
            .filterNot(unmanagedResourceDirectories.value.contains)
            .distinct
        }
      })
    )

Compile / mainClass := (mainClass in assembly.jvm in Compile).value
