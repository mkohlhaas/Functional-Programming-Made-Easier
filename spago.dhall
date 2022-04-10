{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
