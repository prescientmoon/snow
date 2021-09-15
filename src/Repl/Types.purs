module Snow.Repl.Types where

import Snow.Ast (Expr)
import Snow.Type (SnowType)

data Command
  = TypeOf Expr
  | Subsumes SnowType SnowType

