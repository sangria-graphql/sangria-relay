import com.typesafe.tools.mima.core._

name := "sangria-relay"
organization := "org.sangria-graphql"

description := "Sangria Relay Support"
homepage := Some(url("https://sangria-graphql.github.io/"))
licenses := Seq(
  "Apache License, ASL Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))

val isScala3 = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
)

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.18", "2.13.11", "3.3.0")
ThisBuild / scalaVersion := crossScalaVersions.value.tail.head
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility")),
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
)

scalacOptions ++= Seq("-deprecation", "-feature")
scalacOptions += { if (isScala3.value) "-Xtarget:8" else "-target:jvm-1.8" }
javacOptions ++= Seq("-source", "8", "-target", "8")

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "4.0.2",
  "org.scalatest" %% "scalatest" % "3.2.16" % Test)

mimaPreviousArtifacts := {
  if (isScala3.value) Set.empty
  else Set("org.sangria-graphql" %% "sangria-relay" % "2.1.0")
}
mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[IncompatibleResultTypeProblem](
    "sangria.relay.Connection.definitionWithEdge$default$5"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("sangria.relay.Connection.definitionWithEdge")
)

// Publishing
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

startYear := Some(2015)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers :=
  Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) ::
    Developer("yanns", "Yann Simon", "", url("https://github.com/yanns")) ::
    Developer("nickhudkins", "Nick Hudkins", "", url("https://github.com/nickhudkins")) ::
    Developer("sh0hei", "Shohei Shimomura", "", url("https://github.com/sh0hei")) ::
    Nil
scmInfo := Some(
  ScmInfo(
    browseUrl = url("https://github.com/sangria-graphql/sangria-relay"),
    connection = "scm:git:git@github.com:sangria-graphql/sangria-relay.git"
  ))

// nice *magenta* prompt!
ThisBuild / shellPrompt := { state =>
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
