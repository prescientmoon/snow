module Snow.Type where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))

type Existential = { id :: Int, name :: String }

data SnowType
  = Application SnowType SnowType
  | Annotation SnowType SnowType

  ------ Constant types
  | Unit
  | Star Int

  ------ Binders
  | Exists String SnowType SnowType
  | Effectful SnowType SnowType
  | Pi String SnowType SnowType
  | Forall String SnowType SnowType

  ------ Variables
  | Unsolved Existential
  | Universal String

  ------ Value-level constructrs
  | ExprUnit
  | Lambda String SnowType

-- | Apply a monadic function on every layer of a type
everywhereOnTypeM :: forall m. Monad m => (SnowType -> m SnowType) -> SnowType -> m SnowType
everywhereOnTypeM f = f <=< case _ of
  Pi name from to -> Pi name <$> (everywhereOnTypeM f from) <*> (everywhereOnTypeM f to)
  Forall name domain ty -> Forall name <$> (everywhereOnTypeM f domain) <*> (everywhereOnTypeM f ty)
  Exists name domain ty -> Exists name <$> (everywhereOnTypeM f domain) <*> (everywhereOnTypeM f ty)
  Effectful from to -> Effectful <$> (everywhereOnTypeM f from) <*> (everywhereOnTypeM f to)

  ty -> pure ty

-- | Apply a function on every layer of a type
everywhereOnType :: (SnowType -> SnowType) -> SnowType -> SnowType
everywhereOnType f = everywhereOnTypeM (f >>> Identity) >>> unwrap

-- | Subsitutte an existential everywhere inside a type
substitute :: Int -> SnowType -> SnowType -> SnowType
substitute id with = everywhereOnType case _ of
  Unsolved e | e.id == id -> with
  ty -> ty

-- | Substitute an universal everywhere inside a type
substituteUniversal :: String -> SnowType -> SnowType -> SnowType
substituteUniversal target with = case _ of
  Pi name from to ->
    Pi name
      (substituteUniversal target with from)
      (substituteUniversal target with to)
  Universal name | name == target -> with
  Forall name domain ty | name /= target ->
    Forall name
      (substituteUniversal target with domain)
      (substituteUniversal target with ty)
  Exists name domain ty | name /= target ->
    Exists name
      (substituteUniversal target with domain)
      (substituteUniversal target with ty)
  Effectful effect ty ->
    Effectful
      (substituteUniversal target with effect)
      (substituteUniversal target with ty)
  ty -> ty

-- | Checks if a type contains an exitential
occurs :: Existential -> SnowType -> Boolean
occurs target = go
  where
  go = case _ of
    Pi name from to -> go from || go to
    Forall _ domain ty -> go domain || go ty
    Exists _ domain ty -> go domain || go ty
    Effectful effect ty -> go effect || go ty
    Unsolved existential -> target.id == existential.id
    _ -> false

--------- Typeclass instances
derive instance eqType :: Eq SnowType
derive instance genericType :: Generic SnowType _
instance debugType :: Debug SnowType where
  debug ty = genericDebug ty
instance Show SnowType where
  show = printType

---------- Pretty printing
printType :: SnowType -> String
printType Unit = "Unit"
printType (Star level) = if level == 0 then "*" else "*/" <> show level
printType ExprUnit = "unit"
printType (Universal var) = var
printType (Unsolved var) = "?" <> var.name
printType (Application left right) = parensWhen leftNeedsParens printType left <> " " <> parensWhen rightNeedsParens printType right
  where
  leftNeedsParens = case _ of
    Pi _ _ _ -> true
    Forall _ _ _ -> true
    Exists _ _ _ -> true
    Effectful _ _ -> true
    Lambda _ _ -> true
    Annotation _ _ -> true
    _ -> false

  rightNeedsParens = case _ of
    Pi _ _ _ -> true
    Application _ _ -> true
    Annotation _ _ -> true
    _ -> false
printType effect@(Effectful _ _) = "with " <> case effects of
  [ effect ] -> printType effect <> ". " <> printType inner
  _ -> "(" <> joinWith ", " (effects <#> printType) <> "). " <> printType inner
  where
  effects /\ inner = collectEffects effect
printType forall_@(Forall _ _ _) = "forall " <> joinWith " " (variables <#> printBinder) <> ". " <> printType inner
  where
  variables /\ inner = collectForalls forall_
printType exists@(Exists _ _ _) = "exists " <> joinWith " " (variables <#> printBinder) <> ". " <> printType inner
  where
  variables /\ inner = collectExistentials exists
printType (Annotation expr ty) = parensWhen needsParens printType expr <> " :: " <> show ty
  where
  needsParens = case _ of
    Annotation _ _ -> true
    Lambda _ _ -> true
    Forall _ _ _ -> true
    Exists _ _ _ -> true
    Effectful _ _ -> true
    _ -> false
printType lam@(Lambda _ _) = "\\" <> joinWith " " arguments <> " -> " <> printType body
  where
  arguments /\ body = collectLambdas lam
printType (Pi name from to) = "pi " <> printBinder (name /\ from) <> " -> " <> printType to

printBinder :: String /\ SnowType -> String
printBinder (name /\ domain) =
  if domain == Star 0 then
    name
  else
    "(" <> name <> ": " <> printType domain <> ")"

collectMany :: forall t. (SnowType -> Maybe (t /\ SnowType)) -> SnowType -> Array t /\ SnowType
collectMany extract type_ = case extract type_ of
  Just (var /\ inner) -> ([ var ] <> innermostVars) /\ innermostType
    where
    innermostVars /\ innermostType = collectMany extract inner
  Nothing -> [] /\ type_

collectEffects :: SnowType -> Array SnowType /\ SnowType
collectEffects = collectMany case _ of
  Effectful effect inner -> Just (effect /\ inner)
  _ -> Nothing

collectForalls :: SnowType -> Tuple (Array (String /\ SnowType)) SnowType
collectForalls = collectMany case _ of
  Forall var domain inner -> Just ((var /\ domain) /\ inner)
  _ -> Nothing

collectExistentials :: SnowType -> Tuple (Array (String /\ SnowType)) SnowType
collectExistentials = collectMany case _ of
  Exists var domain inner -> Just ((var /\ domain) /\ inner)
  _ -> Nothing

collectLambdas :: SnowType -> Array String /\ SnowType
collectLambdas = collectMany case _ of
  Lambda argument body -> Just (argument /\ body)
  _ -> Nothing

parensWhen :: forall a. (a -> Boolean) -> (a -> String) -> a -> String
parensWhen predicate print toPrint = if predicate toPrint then "(" <> print toPrint <> ")" else print toPrint