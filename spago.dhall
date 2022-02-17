{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "integers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
