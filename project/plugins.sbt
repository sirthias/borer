addSbtPlugin("org.scalameta"         % "sbt-scalafmt"               % "2.5.0")
addSbtPlugin("io.crashbox"           % "sbt-gpg"                    % "0.2.1")
addSbtPlugin("com.github.sbt"        % "sbt-release"                % "1.1.0")
addSbtPlugin("org.xerial.sbt"        % "sbt-sonatype"               % "3.9.19")
addSbtPlugin("de.heikoseeberger"     % "sbt-header"                 % "5.9.0")
addSbtPlugin("io.spray"              % "sbt-boilerplate"            % "0.6.1")
addSbtPlugin("org.scala-js"          % "sbt-scalajs"                % "1.13.1")
addSbtPlugin("org.portable-scala"    % "sbt-scalajs-crossproject"   % "1.3.1")
addSbtPlugin("pl.project13.scala"    % "sbt-jmh"                    % "0.4.4")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.10.3")
addSbtPlugin("io.bullet"             % "sbt-paradox-material-theme" % "0.7.0")
addSbtPlugin("com.github.sbt"        % "sbt-ghpages"                % "0.7.0")
addSbtPlugin("com.typesafe.sbt"      % "sbt-site"                   % "1.4.1")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.7.2",
  "io.bullet" %% "borer-derivation" % "1.7.2"
)
