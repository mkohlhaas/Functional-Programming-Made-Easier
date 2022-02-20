{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "aff", "avar", "datetime", "enums", "exceptions", "random", "aff-bus", "transformers", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
