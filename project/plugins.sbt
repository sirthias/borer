addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.6.2")
addSbtPlugin("com.github.sbt"     % "sbt-pgp"                       % "2.3.2")
addSbtPlugin("com.github.sbt"     % "sbt-release"                   % "1.6.0")
addSbtPlugin("com.github.sbt"     % "sbt-header"                    % "5.11.0")
addSbtPlugin("com.github.sbt"     % "sbt-boilerplate"               % "0.8.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.23.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.4.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.8")
addSbtPlugin("com.github.sbt"     % "sbt-paradox-material-theme"    % "0.8.0")
addSbtPlugin("com.github.sbt"     % "sbt-ghpages"                   % "0.10.0")
addSbtPlugin("com.github.sbt"     % "sbt-site-paradox"              % "1.8.0")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.19.0",
  "io.bullet" %% "borer-derivation" % "1.19.0"
)
