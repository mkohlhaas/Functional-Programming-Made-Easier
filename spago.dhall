{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "unicode", "control", "either", "foldable-traversable", "maybe", "strings", "tuples", "unfoldable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
