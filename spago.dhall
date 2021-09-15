{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "debugged"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "parsing"
  , "partial"
  , "prelude"
  , "psci-support"
  , "run"
  , "run-supply"
  , "strings"
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
