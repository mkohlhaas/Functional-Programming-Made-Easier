{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "maybe" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
