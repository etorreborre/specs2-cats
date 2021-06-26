// release management
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.12.0")
addSbtPlugin("com.geirsson"   % "sbt-ci-release"     % "1.5.7")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)
resolvers += Resolver.sonatypeRepo("snapshots")
