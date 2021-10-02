module Snow.Repl.Types where

import Snow.Type (SnowType)

data Command
  = TypeOf SnowType
  | Subsumes SnowType SnowType
  | Assume String SnowType

