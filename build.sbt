ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

// Define versions for the dependencies
val junitVersion = "4.13.2" // Replace with the desired JUnit version
val guavaVersion = "31.1-jre" // Replace with the desired Guava version

lazy val root = (project in file("."))
  .settings(
    name := "scala-poker",

    // Corrected library dependencies
    libraryDependencies ++= Seq(
      "junit" % "junit" % junitVersion % Test, // Corrected JUnit dependency for testing
      "com.google.guava" % "guava" % guavaVersion, // Guava dependency
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test// Add this for better JUnit support in Scala
    )
  )
