addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.5.4")
addSbtPlugin("io.crashbox"        % "sbt-gpg"                       % "0.2.1")
addSbtPlugin("com.github.sbt"     % "sbt-release"                   % "1.4.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"                  % "3.11.2")
addSbtPlugin("de.heikoseeberger"  % "sbt-header"                    % "5.10.0")
addSbtPlugin("com.github.sbt"     % "sbt-boilerplate"               % "0.7.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.17.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.6")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.7")
addSbtPlugin("io.bullet"          % "sbt-paradox-material-theme"    % "0.7.0")
addSbtPlugin(("com.github.sbt" % "sbt-ghpages"      % "0.7.0").exclude("org.scala-lang.modules", "scala-xml_2.12"))
addSbtPlugin("com.github.sbt"  % "sbt-site-paradox" % "1.7.0")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.7.2",
  "io.bullet" %% "borer-derivation" % "1.7.2"
)
