name := "3DCNN"

version := "0.1"

scalaVersion := "2.11.12"
val spinalVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9"

fork := true


//breeze
libraryDependencies += "org.scalanlp" %% "breeze" % "1.0"


//rings
libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.7"
