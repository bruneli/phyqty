
name := "phyqty"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra :=
  <url>https://github.com/bruneli/phyqty</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:bruneli/phyqty.git</url>
      <connection>scm:git:git@github.com:bruneli/phyqty.git</connection>
    </scm>
    <developers>
      <developer>
        <id>bruneli</id>
        <name>Renaud Bruneliere</name>
        <url>https://github.com/bruneli</url>
      </developer>
    </developers>

lazy val commonSettings = Seq(
  organization := "com.github.bruneli.phyqty",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8")
)

lazy val root = project.in(file(".")).settings(commonSettings: _*)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6"
