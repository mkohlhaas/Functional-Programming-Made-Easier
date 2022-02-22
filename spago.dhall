{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "console"
  , "effect"
  , "either"
  , "foreign"
  , "foreign-generic"
  , "maybe"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
