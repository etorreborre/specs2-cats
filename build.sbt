/** PROJECT */
lazy val specs2Cats = project
  .in(file("."))
  .enablePlugins(GitBranchPrompt, GitVersioning)
  .settings(
    name := "specs2-cats-root",
    rootSettings
  )
  .aggregate(
    cats.jvm,
    cats.js,
    catsEffect.jvm,
    catsEffect.js,
  )

val platforms = List(JVMPlatform, JSPlatform)
val jvm = JVMPlatform

lazy val cats = crossProject(platforms: _*)
  .crossType(CrossType.Pure)
  .withoutSuffixFor(jvm)
  .in(file("cats"))
  .settings(
    organization := "org.specs2",
    name := "specs2-cats",
    catsDependencies,
    buildSettings
  )
  .jvmSettings(buildJvmSettings)
  .jsSettings(buildJsSettings)

lazy val catsEffect = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("cats-effect"))
  .settings(
    organization := "org.specs2",
    name := "specs2-cats-effect",
    catsEffectDependencies,
    buildSettings
  )
  .jvmSettings(buildJvmSettings)
  .jsSettings(buildJsSettings)
  .dependsOn(cats)

/** SETTINGS */

val Scala3 = "3.0.2"
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / scalaVersion := Scala3

val catsDependencies = libraryDependencies ++= Seq(
  "org.specs2" %%% "specs2-core" % "5.0.0-RC-10",
  "org.typelevel" %%% "cats-core" % "2.6.1"
)
val catsEffectDependencies = libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.2.8"

lazy val rootSettings =
  compilationSettings ++
    testingSettings ++
    releaseSettings ++
    Seq(
      packagedArtifacts := Map.empty,
      test := {},
      mimaPreviousArtifacts := Set.empty
    )

lazy val buildSettings =
  compilationSettings ++
    testingSettings ++
    releaseSettings

lazy val buildJvmSettings = testingJvmSettings
lazy val buildJsSettings = testingJsSettings

lazy val compilationSettings = Seq(
  maxErrors := 20,
  Global / onChangedBuildSource := ReloadOnSourceChanges,
  Compile / scalacOptions ++= compilationOptions,
  Compile / doc / scalacOptions ++= compilationOptions
)

lazy val compilationOptions = Seq(
  "-source:future-migration",
  "-language:implicitConversions,postfixOps",
  "-Ykind-projector",
  "-Xcheck-macros",
  "-deprecation:false",
  "-unchecked",
  "-feature"
)

lazy val testingSettings = Seq(
  logBuffered := false,
  Global / cancelable := true,
  testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
  testOptions := Seq(Tests.Filter(s => Seq("Spec").exists(s.endsWith)))
)

lazy val testingJvmSettings = Seq(javaOptions ++= Seq("-Xmx3G", "-Xss4M"), Test / fork := true)

lazy val testingJsSettings = Seq(
  Test / fork := false,
  Test / parallelExecution := false,
  Test / scalaJSStage := FastOptStage
)

/** RELEASE
  */
lazy val releaseSettings: Seq[Setting[_]] = Seq(
  ThisBuild / versionScheme := Some("early-semver"),
  ThisBuild / githubWorkflowArtifactUpload := false,
  ThisBuild / githubWorkflowBuild := Seq(
    WorkflowStep.Sbt(name = Some("Build and test 🔧"), commands = List("testOnly -- xonly exclude ci timefactor 3"))
  ),
  ThisBuild / githubWorkflowTargetTags ++= Seq(SPECS2 + "*"),
  ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag(SPECS2))),
  ThisBuild / githubWorkflowPublish := Seq(
    WorkflowStep.Sbt(
      name = Some("Release to Sonatype 📇"),
      commands = List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    )
  ),
  homepage := Some(url("https://github.com/etorreborre/specs2-cats")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "etorreborre",
      "Eric Torreborre",
      "etorreborre@yahoo.com",
      url("https://github.com/etorreborre")
    )
  ),
  ThisBuild / git.gitTagToVersionNumber := { tag: String =>
    if (tag matches SPECS2 + ".*") Some(tag.replace(SPECS2, "")) else None
  },
  ThisBuild / dynverTagPrefix := SPECS2
)

val SPECS2 = "SPECS2-CATS-"
