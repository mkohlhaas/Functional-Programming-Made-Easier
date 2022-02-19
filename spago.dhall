{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "aff", "avar", "datetime", "enums", "exceptions", "random", "aff-bus" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
