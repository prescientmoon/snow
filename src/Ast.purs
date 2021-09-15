module Snow.Ast where

import Prelude
import Data.Tuple.Nested ((/\), type (/\))
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Snow.Type (SnowType, parensWhen)

data Expr
  = ExprUnit
  | ExprVariable String
  | ExprAnnotation Expr SnowType
  | ExprLambda String Expr
  | ExprCall Expr Expr

---------- Helpers
printExpr :: Expr -> String
printExpr ExprUnit = "unit"
printExpr (ExprVariable name) = name
printExpr (ExprAnnotation expr ty) = parensWhen needsParens printExpr expr <> " :: " <> show ty
  where
  needsParens = case _ of
    ExprAnnotation _ _ -> true
    ExprLambda _ _ -> true
    _ -> false
printExpr lam@(ExprLambda _ _) = "\\" <> joinWith " " arguments <> " -> " <> printExpr body
  where
  arguments /\ body = collectLambdas lam
printExpr (ExprCall left right) = parensWhen parensLeft printExpr left <> " " <>
  parensWhen parensRight printExpr right
  where
  parensLeft = case _ of
    ExprAnnotation _ _ -> true
    ExprLambda _ _ -> true
    _ -> false

  parensRight = case _ of
    ExprCall _ _ -> true
    ExprAnnotation _ _ -> true
    _ -> false

collectLambdas :: Expr -> Array String /\ Expr
collectLambdas (ExprLambda argument body) = ([ argument ] <> arguments) /\ inner
  where
  arguments /\ inner = collectLambdas body
collectLambdas other = [] /\ other

---------- Typeclass isnances
derive instance Generic Expr _
instance Debug Expr where
  debug a = genericDebug a

instance Show Expr where
  show = printExpr
