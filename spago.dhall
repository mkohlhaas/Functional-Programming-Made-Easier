{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
