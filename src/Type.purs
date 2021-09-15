module Snow.Type where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

type Existential = { id :: Int, name :: String }

data SnowType
  = Unit
  | Function SnowType SnowType
  | Forall String SnowType
  | Exists String SnowType
  | Existential Existential
  | Universal String

-- | Apply a monadic function on every layer of a type
everywhereOnTypeM :: forall m. Monad m => (SnowType -> m SnowType) -> SnowType -> m SnowType
everywhereOnTypeM f = f <=< case _ of
  Function from to -> Function <$> (everywhereOnTypeM f from) <*> (everywhereOnTypeM f to)
  Forall name ty -> Forall name <$> (everywhereOnTypeM f ty)
  Exists name ty -> Exists name <$> (everywhereOnTypeM f ty)
  ty -> pure ty

-- | Apply a function on every layer of a type
everywhereOnType :: (SnowType -> SnowType) -> SnowType -> SnowType
everywhereOnType f = everywhereOnTypeM (f >>> Identity) >>> unwrap

-- | Subsitutte an existential everywhere inside a type
substitute :: Int -> SnowType -> SnowType -> SnowType
substitute id with = everywhereOnType case _ of
  Existential e | e.id == id -> with
  ty -> ty

-- | Substitute an universal everywhere inside a type
substituteUniversal :: String -> SnowType -> SnowType -> SnowType
substituteUniversal target with = case _ of
  Function from to ->
    Function
      (substituteUniversal target with from)
      (substituteUniversal target with to)
  Universal name | name == target -> with
  Forall name ty | name /= target ->
    substituteUniversal target with ty
  Exists name ty | name /= target ->
    substituteUniversal target with ty
  ty -> ty

-- | Checks if a type contains an exitential
occurs :: Existential -> SnowType -> Boolean
occurs target = go
  where
  go = case _ of
    Function from to -> go from || go to
    Forall _ ty -> go ty
    Exists _ ty -> go ty
    Existential existential -> target.id == existential.id
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
printType (Universal var) = var
printType (Existential var) = "?" <> var.name
printType forall_@(Forall _ _) = "forall " <> joinWith " " variables <> "." <> printType inner
  where
  variables /\ inner = collectForalls forall_
printType exists@(Exists _ _) = "exists " <> joinWith " " variables <> "." <> printType inner
  where
  variables /\ inner = collectExistentials exists
printType (Function from to) = parensWhen fromNeedsParens printType from <> " -> " <> parensWhen toNeedsParens printType to
  where
  fromNeedsParens = case _ of
    Function _ _ -> true
    Forall _ _ -> true
    _ -> false

  toNeedsParens = const false

collectForalls :: SnowType -> Tuple (Array String) SnowType
collectForalls (Forall var inner) = ([ var ] <> innermostVars) /\ innermostType
  where
  innermostVars /\ innermostType = collectForalls inner
collectForalls other = [] /\ other

collectExistentials :: SnowType -> Tuple (Array String) SnowType
collectExistentials (Exists var inner) = ([ var ] <> innermostVars) /\ innermostType
  where
  innermostVars /\ innermostType = collectForalls inner
collectExistentials other = [] /\ other

parensWhen :: forall a. (a -> Boolean) -> (a -> String) -> a -> String
parensWhen predicate print toPrint = if predicate toPrint then "(" <> print toPrint <> ")" else print toPrint