name := "bunny"

version := "1.0.0"

libraryDependencies += "org.fusesource.scalate" % "scalate-core" % "1.5.3"

// for test
libraryDependencies += "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"

libraryDependencies += "org.jmock" % "jmock" % "2.5.1" % "test"

scalacOptions += "-deprecation"
