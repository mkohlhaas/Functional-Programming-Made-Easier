{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
