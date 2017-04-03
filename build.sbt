name := "SRLRepresentationLearning"

version := "1.2.2"

scalaVersion := "2.11.7"

mainClass in assembly := some("representationLearning.LearnNewRepresentation")
assemblyJarName := "LearnNewRepresentation.jar"

libraryDependencies += "org.clapper" %% "argot" % "1.0.3"

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.11.2",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  // the visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  //visualization
  "org.vegas-viz" %% "vegas" % "0.3.8"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}