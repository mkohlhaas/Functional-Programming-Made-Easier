{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foreign-generic"
  , "httpure"
  , "maybe"
  , "prelude"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
