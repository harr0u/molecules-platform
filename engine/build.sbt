ThisBuild / scalaVersion := "2.13.1"

ThisBuild / organization := "com.molecules"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.8.3" % "test")

libraryDependencies += "org.apache.kafka" % "kafka_2.12" % "2.1.0"

scalacOptions in Test += "-Yrangepos"

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "1.0",

  // Native libraries are not included by default. add this if you want them
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.0",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "1.0"
)