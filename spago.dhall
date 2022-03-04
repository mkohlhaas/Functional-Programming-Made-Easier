{ name = "my-project"
, dependencies = [ "arrays", "console", "effect", "functions", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
