
val zioVersion = "2.0.13"
val zioSchemaVersion = "0.4.14"

val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        organization := "Nitneckstudio",
        scalaVersion := "2.13.10"
      )
    ),
    name           := "type-safe-decoders-without-boilerplate",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    libraryDependencies ++= Seq(
      // general
      "dev.zio"       %% "zio"                   % zioVersion,
      "dev.zio"       %% "zio-schema"            % zioSchemaVersion,
      "dev.zio"       %% "zio-schema-bson"       % zioSchemaVersion,
      "dev.zio"       %% "zio-schema-json"       % zioSchemaVersion,
      "dev.zio"       %% "zio-schema-protobuf"   % zioSchemaVersion,
      "dev.zio"       %% "zio-schema-optics"     % zioSchemaVersion,
      // Required for automatic generic derivation of schemas
      "dev.zio"       %% "zio-schema-derivation" % zioSchemaVersion,
      "org.scala-lang" % "scala-reflect"         % scalaVersion.value % "provided",
      // test
      "dev.zio"       %% "zio-test-sbt"          % zioVersion         % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
