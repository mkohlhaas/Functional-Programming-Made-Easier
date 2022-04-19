{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
