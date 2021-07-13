import sbt._
import sbt.Keys._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val circeVersion = "0.14.1"

scalaVersion := "2.13.6"

lazy val assembly =
  crossProject(JSPlatform, JVMPlatform)
    .settings(
        name := "assembly",

        scalaVersion := "2.13.6",

        organization := "com.github.antonkulaga",

	      version := "0.0.14",

        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

        exportJars := true,

        run / fork := true,

        parallelExecution in Test := false,

        licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),

        resolvers += "jitpack.io" at "https://jitpack.io",

        libraryDependencies ++= Seq(
            "io.circe" %%% "circe-core"% circeVersion,
            "io.circe" %%% "circe-generic"% circeVersion,
            "io.circe" %%% "circe-generic-extras"% circeVersion,
            "io.circe" %%% "circe-parser"% circeVersion,
            "org.scalatest" %%% "scalatest" % "3.1.2" % Test
        ),

      scalacOptions ++= Seq("-feature", "-language:_", "-Ymacro-annotations"),

      //addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

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
