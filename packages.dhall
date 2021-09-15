let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210825/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f

let additions =
      { debugged =
        { dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "datetime"
          , "enums"
          , "unordered-collections"
          ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-debugged"
        , version = "master"
        }
      }

in  additions // upstream
