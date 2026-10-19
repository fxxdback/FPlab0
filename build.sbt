name := "password-generator"
version := "1.0"
scalaVersion := "3.3.4"

libraryDependencies += "com.lihaoyi" %% "ujson" % "4.0.2"
fork := true
run / connectInput := true
outputStrategy := Some(StdoutOutput)
