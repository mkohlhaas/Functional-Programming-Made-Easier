{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "tuples", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
