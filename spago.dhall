{ name = "my-project"
, dependencies = [ "bifunctors", "console", "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
