{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "either", "tuples", "unicode", "arrays", "control", "foldable-traversable", "maybe", "strings", "unfoldable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
