{ name = "my-project"
, dependencies =
  [ "console", "effect", "foldable-traversable", "lists", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
