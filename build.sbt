name := "recursive-trees"
version := "0.1"
scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.typelevel" %% "cats-effect" % "2.+",
  "io.monix" %% "monix" % "3.+",
  "org.jsoup" % "jsoup" % "1.13.1",
)