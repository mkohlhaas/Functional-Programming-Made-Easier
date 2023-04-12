{ name = "my-project"
, dependencies = [ "console", "effect", "integers", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
