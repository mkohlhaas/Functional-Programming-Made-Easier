{ name = "my-project"
, dependencies = [ "console", "effect", "either", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
