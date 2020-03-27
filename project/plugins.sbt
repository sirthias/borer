addSbtPlugin("org.scalameta"          % "sbt-scalafmt"                % "2.3.2")
addSbtPlugin("io.crashbox"            % "sbt-gpg"                     % "0.2.1")
addSbtPlugin("com.github.gseitz"      % "sbt-release"                 % "1.0.13")
addSbtPlugin("org.xerial.sbt"         % "sbt-sonatype"                % "3.9.1")
addSbtPlugin("de.heikoseeberger"      % "sbt-header"                  % "5.4.0")
addSbtPlugin("io.spray"               % "sbt-boilerplate"             % "0.6.1")
addSbtPlugin("org.scala-js"           % "sbt-scalajs"                 % "1.0.1")
addSbtPlugin("org.portable-scala"     % "sbt-scalajs-crossproject"    % "1.0.0")
addSbtPlugin("pl.project13.scala"     % "sbt-jmh"                     % "0.3.7")
addSbtPlugin("org.scoverage"          % "sbt-scoverage"               % "1.6.1")
addSbtPlugin("com.lightbend.paradox"  % "sbt-paradox"                 % "0.6.9")
addSbtPlugin("io.bullet"              % "sbt-paradox-material-theme"  % "0.7.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-ghpages"                 % "0.6.3")
addSbtPlugin("com.typesafe.sbt"       % "sbt-site"                    % "1.4.0")
addSbtPlugin("com.typesafe"           % "sbt-mima-plugin"             % "0.7.0")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.5.0",
  "io.bullet" %% "borer-derivation" % "1.5.0"
)