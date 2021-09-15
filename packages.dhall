let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210825/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f

let additions =
      { debugged =
        { dependencies =
            ( https://raw.githubusercontent.com/Mateiadrielrafael/purescript-debugged/master/spago.dhall sha256:21e19b2fda8466203706b8a4543f65cf9fb21bac9f8a9d914853162ce4611e00
            ).dependencies
        , repo = "https://github.com/Mateiadrielrafael/purescript-debugged"
        , version = "633220f91f87c9acbc4eebbf87628e6cdc658b7b"
        }
      , run-supply =
        { dependencies =
            ( https://raw.githubusercontent.com/Mateiadrielrafael/purescript-run-supply/main/spago.dhall sha256:8f58c7c5af2944c7d66936d2c67ffd5fcd789b480d49a867bdd10fe88d68c161
            ).dependencies
        , repo = "https://github.com/Mateiadrielrafael/purescript-run-supply"
        , version = "main"
        }
      }

in  additions // upstream
