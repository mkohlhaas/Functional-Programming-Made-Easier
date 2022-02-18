{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "node-buffer", "node-fs", "either", "exceptions", "aff", "node-fs-aff", "transformers", "datetime", "avar", "aff-bus", "strings", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
