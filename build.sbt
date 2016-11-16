name := "text-classification"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("com.gmail.pderichai.text.classification.Main")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
