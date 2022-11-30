{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
