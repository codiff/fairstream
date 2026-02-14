// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "com.codiff"
ThisBuild / organizationName := "codiff"
ThisBuild / startYear := Some(2026)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("amir", "Amir Saeid")
)

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

val Scala213 = "2.13.18"
ThisBuild / crossScalaVersions := Seq(Scala213, "3.3.7")
ThisBuild / scalaVersion := Scala213 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core, fs2)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "fairstream",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-effect" % "3.6.3",
      "org.scalameta" %%% "munit" % "1.2.2" % Test,
      "org.typelevel" %%% "munit-cats-effect" % "2.1.0" % Test
    )
  )

lazy val fs2 = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("fs2"))
  .dependsOn(core)
  .settings(
    name := "fairstream-fs2",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % "3.12.2",
      "org.scalameta" %%% "munit" % "1.2.2" % Test,
      "org.typelevel" %%% "munit-cats-effect" % "2.1.0" % Test
    )
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
