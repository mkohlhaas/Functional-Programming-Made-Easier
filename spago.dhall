{ name = "my-project"
, dependencies =
  [ "console", "effect", "either", "invariant", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
