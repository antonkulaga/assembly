import sbt._
import sbt.Keys._
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val circeVersion = "0.12.3"

scalaVersion := "2.13.1"

lazy val assembly =
  crossProject(JSPlatform, JVMPlatform)
    .settings(
        name := "assembly",

        scalaVersion := "2.13.1",

        organization := "group.aging-research",

	      version := "0.0.13",

        resolvers += sbt.Resolver.bintrayRepo("comp-bio-aging", "main"),

        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

        exportJars := true,

        fork in run := true,

        parallelExecution in Test := false,

        bintrayRepository := "main",

        bintrayOrganization := Some("comp-bio-aging"),

        licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
        
        libraryDependencies ++= Seq(
            "io.circe" %%% "circe-core"% circeVersion,
            "io.circe" %%% "circe-generic"% circeVersion,
            "io.circe" %%% "circe-generic-extras"% "0.12.2",//circeVersion,
            "io.circe" %%% "circe-parser"% circeVersion,
            "org.scalatest" %%% "scalatest" % "3.0.8" % Test
        ),


      scalacOptions ++= Seq("-target:jvm-1.8", "-feature", "-language:_", "-Ymacro-annotations"),

      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

      Seq(Compile, Test, Runtime).flatMap(inConfig(_) {
        unmanagedResourceDirectories ++= {
          unmanagedSourceDirectories.value
            .map(src => (src / ".." / "resources").getCanonicalFile)
            .filterNot(unmanagedResourceDirectories.value.contains)
            .distinct
        }
      })
    )

mainClass in Compile := (mainClass in assembly.jvm in Compile).value
