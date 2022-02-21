{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "avar"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "prelude"
  , "random"
  , "tailrec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
