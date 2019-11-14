name := "recursion-schemes"

version := "0.1"

scalaVersion := "2.12.9"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq("com.slamdata" %% "matryoshka-core" % "0.21.3",
"io.circe" %% "circe-parser" % "0.12.0-RC3",
"io.circe" %% "circe-generic" % "0.12.0-RC3",
"com.sksamuel.avro4s" %% "avro4s-core" % "3.0.0")

import scalariform.formatter.preferences._
scalariformPreferences := scalariformPreferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(SpaceBeforeColon, false)
  .setPreference(CompactStringConcatenation, false)
  .setPreference(PreserveSpaceBeforeArguments, false)
  .setPreference(AlignParameters, true)
  .setPreference(AlignArguments, false)
  .setPreference(DoubleIndentConstructorArguments, false)
  .setPreference(FormatXml, true)
  .setPreference(IndentPackageBlocks, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 50)
  .setPreference(IndentLocalDefs, false)
  .setPreference(DanglingCloseParenthesis, Force)
  .setPreference(SpaceInsideParentheses, false)
  .setPreference(SpaceInsideBrackets, false)
  .setPreference(SpacesWithinPatternBinders, true)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  .setPreference(IndentWithTabs, false)
  .setPreference(CompactControlReadability, false)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  .setPreference(SpacesAroundMultiImports, true)