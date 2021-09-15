module Snow.Ast where

import Snow.Type (SnowType)

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprAnnotation Expr SnowType
  | ExprLambda String Expr
  | ExprCall Expr Expr
