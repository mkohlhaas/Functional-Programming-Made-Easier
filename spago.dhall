{ name = "my-project"
, dependencies =
  [ "console", "effect", "either", "maybe", "prelude", "strings", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
