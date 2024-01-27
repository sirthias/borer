addSbtPlugin("org.scalameta"          % "sbt-scalafmt"               % "2.5.2")
addSbtPlugin("io.crashbox"            % "sbt-gpg"                    % "0.2.1")
addSbtPlugin("com.github.sbt"         % "sbt-release"                % "1.4.0")
addSbtPlugin("org.xerial.sbt"         % "sbt-sonatype"               % "3.10.0")
addSbtPlugin("de.heikoseeberger"      % "sbt-header"                 % "5.10.0")
addSbtPlugin("com.github.sbt"         % "sbt-boilerplate"            % "0.7.0")
addSbtPlugin("org.scala-js"           % "sbt-scalajs"                % "1.15.0")
addSbtPlugin("org.portable-scala"     % "sbt-scalajs-crossproject"   % "1.3.2")
addSbtPlugin("pl.project13.scala"     % "sbt-jmh"                    % "0.4.7")
addSbtPlugin("com.lightbend.paradox"  % "sbt-paradox"                % "0.10.6")
addSbtPlugin("io.bullet"              % "sbt-paradox-material-theme" % "0.7.0")
addSbtPlugin("com.github.sbt"         % "sbt-ghpages"                % "0.7.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-site"                   % "1.4.1")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.7.2",
  "io.bullet" %% "borer-derivation" % "1.7.2"
)
