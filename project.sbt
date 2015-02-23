name := "ild-scala-practice"

description := "A project to practice with Scala"

scalaVersion := "2.11.4"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val ILDScalaPractice = FDProject(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
	"org.scalatest" %% "scalatest" % "[2.2,)" % "test"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"