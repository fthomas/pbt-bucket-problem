name := "pbt-bucket-problem"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
                                      "-minSuccessfulTests",
                                      "10000")
