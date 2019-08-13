addSbtPlugin("org.scalameta"          % "sbt-scalafmt"                % "2.0.2")
addSbtPlugin("io.crashbox"            % "sbt-gpg"                     % "0.2.0")
addSbtPlugin("com.github.gseitz"      % "sbt-release"                 % "1.0.11")
addSbtPlugin("org.xerial.sbt"         % "sbt-sonatype"                % "2.5")
addSbtPlugin("de.heikoseeberger"      % "sbt-header"                  % "5.2.0")
addSbtPlugin("io.spray"               % "sbt-boilerplate"             % "0.6.1")
addSbtPlugin("org.scala-js"           % "sbt-scalajs"                 % "0.6.28")
addSbtPlugin("org.portable-scala"     % "sbt-scalajs-crossproject"    % "0.6.1")
addSbtPlugin("pl.project13.scala"     % "sbt-jmh"                     % "0.3.7")
addSbtPlugin("org.scoverage"          % "sbt-scoverage"               % "1.6.0")
addSbtPlugin("com.lightbend.paradox"  % "sbt-paradox"                 % "0.6.0")
addSbtPlugin("io.bullet"              % "sbt-paradox-material-theme"  % "0.7.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-ghpages"                 % "0.6.3")
addSbtPlugin("com.typesafe.sbt"       % "sbt-site"                    % "1.4.0")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "0.11.1",
  "io.bullet" %% "borer-derivation" % "0.11.1"
)