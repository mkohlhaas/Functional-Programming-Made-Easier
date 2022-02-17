{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "arrays", "either", "foldable-traversable", "maybe", "strings", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
