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
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
