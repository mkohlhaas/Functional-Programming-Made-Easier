{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "strings"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
