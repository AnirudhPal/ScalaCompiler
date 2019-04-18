name := "CMScala-compiler"

version := "1.0"

organization := "Purdue"


/**
 *  The main sbt configuration for the compiler build.
 */

lazy val defaults = Seq(
  // scala compiler version:
  scalaVersion := "2.11.12",
  scalaBinaryVersion := "2.11",
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),

  // source location configuration:
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Compile := baseDirectory.value / "resources",

  // eclipse configuration:
  unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
  unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),

  // run configuration
  fork in run := false,
  connectInput in run := false,
  outputStrategy := Some(StdoutOutput),
  javaOptions in run ++= Seq("-Xss32M", "-Xms128M"),

  // test configuration:
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),
  parallelExecution in Test := false,
  // fork in test
  fork in Test := true,
  javaOptions in Test ++= Seq("-Djava.security.manager", "-Djava.security.policy=" + baseDirectory.value + "/project/tests.policy"),
  javaOptions in Test ++= Seq("-Xss32M", "-Xms128M"),

  // packaging configuration (also include tests, create submit-me.jar):
  unmanagedSources in (Compile, packageSrc) ++= (unmanagedSources in Test).value,
  mappings in (Compile, packageSrc) := {
    (unmanagedSources in (Compile, packageSrc)).value pair Path.relativeTo(baseDirectory.value)
  },
  artifactPath in (Compile, packageSrc) := submitMe.value,
  cleanFiles += submitMe.value
)

// The location desired for the source jar that will be submitted
def submitMe = baseDirectory(_ / "submit-me.jar")

lazy val cmScala = (project in file(".")).settings(
  defaults,
  name := "CMScala")
