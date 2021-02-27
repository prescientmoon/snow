{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "debugged"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "parsing"
  , "partial"
  , "psci-support"
  , "run"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
