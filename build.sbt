import sbt._
import sbt.Keys._
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val circeVersion = "0.10.1"

lazy val assembly =
  crossProject(JSPlatform, JVMPlatform)
    .settings(
        name := "assembly",

        scalaVersion := "2.12.7",

        organization := "group.aging-research",

	      version := "0.0.4",

        resolvers += sbt.Resolver.bintrayRepo("comp-bio-aging", "main"),

        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),

        exportJars := true,

        fork in run := true,

        parallelExecution in Test := false,

        bintrayRepository := "main",

        bintrayOrganization := Some("comp-bio-aging"),

        licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
        
        libraryDependencies ++= Seq(
            "org.typelevel" %%% "cats-core" % "1.4.0",
            "org.wvlet.airframe" %%% "airframe-log" % "0.74",
            "io.circe" %%% "circe-core"% circeVersion,
            "io.circe" %%% "circe-generic"% circeVersion,
            "io.circe" %%% "circe-generic-extras"% circeVersion,
            "io.circe" %%% "circe-parser"% circeVersion,
            "org.scalatest" %%% "scalatest" % "3.0.5" % Test
        ),
      addCompilerPlugin(
        "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
      ),
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
