{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "httpure"
  , "identity"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "parsing"
  , "posix-types"
  , "prelude"
  , "strings"
  , "stringutils"
  , "transformers"
  , "unicode"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
