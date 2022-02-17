{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "either", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
