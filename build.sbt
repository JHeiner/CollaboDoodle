
organization := "com.heivo"

name := "liftfromscratch"

version := "0.1"

scalaVersion := "2.10.0"

seq(com.github.siasia.WebPlugin.webSettings :_*)

libraryDependencies ++= {
  val liftVersion = "2.5-M4"
  Seq(
	"net.liftweb" %% "lift-webkit" % liftVersion % "compile",
	"org.eclipse.jetty" % "jetty-webapp" % "8.1.9.v20130131" % "container,test",
	"org.slf4j" % "slf4j-jdk14" % "1.7.2" % "container,test",
	"net.liftweb" %% "lift-mongodb-record" % liftVersion ) }

