{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "lists" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
