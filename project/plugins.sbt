addSbtPlugin("com.eed3si9n"          % "sbt-projectmatrix"          % "0.9.0")
addSbtPlugin("org.scalameta"         % "sbt-scalafmt"               % "2.4.5")
addSbtPlugin("io.crashbox"           % "sbt-gpg"                    % "0.2.1")
addSbtPlugin("com.github.sbt"        % "sbt-release"                % "1.1.0")
addSbtPlugin("org.xerial.sbt"        % "sbt-sonatype"               % "3.9.10")
addSbtPlugin("de.heikoseeberger"     % "sbt-header"                 % "5.6.0")
addSbtPlugin("io.spray"              % "sbt-boilerplate"            % "0.6.1")
addSbtPlugin("org.scala-js"          % "sbt-scalajs"                % "1.8.0")
addSbtPlugin("pl.project13.scala"    % "sbt-jmh"                    % "0.4.3")
addSbtPlugin("org.scoverage"         % "sbt-scoverage"              % "1.9.2")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox"                % "0.9.2")
addSbtPlugin("io.bullet"             % "sbt-paradox-material-theme" % "0.7.0")
addSbtPlugin("com.typesafe.sbt"      % "sbt-ghpages"                % "0.6.3")
addSbtPlugin("com.typesafe.sbt"      % "sbt-site"                   % "1.4.1")
addSbtPlugin("com.typesafe"          % "sbt-mima-plugin"            % "1.0.1")

libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.7.2",
  "io.bullet" %% "borer-derivation" % "1.7.2"
)
