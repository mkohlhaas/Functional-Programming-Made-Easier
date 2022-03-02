{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foreign-generic"
  , "maybe"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
