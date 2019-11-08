name := "recursion-schemes"

version := "0.1"

scalaVersion := "2.12.9"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq("com.slamdata" %% "matryoshka-core" % "0.21.3",
"io.circe" %% "circe-parser" % "0.12.0-RC3",
"io.circe" %% "circe-generic" % "0.12.0-RC3",
"com.sksamuel.avro4s" %% "avro4s-core" % "3.0.0")