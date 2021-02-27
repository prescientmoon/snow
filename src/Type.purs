module Type where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

type Existential = { id :: Int, name :: String }

data Type 
    = Unit
    | Function Type Type
    | Forall String Type
    | Existential Existential
    | Universal String

-- | Apply a monadic function on every layer of a type
everywhereOnTypeM :: forall m. Monad m => (Type -> m Type) -> Type -> m Type
everywhereOnTypeM f = f <=< case _ of
    Function from to -> Function <$> (everywhereOnTypeM f from) <*> (everywhereOnTypeM f to)
    Forall name ty -> Forall name <$> (everywhereOnTypeM f ty)
    ty -> pure ty

-- | Apply a function on every layer of a type
everywhereOnType :: (Type -> Type) -> Type -> Type
everywhereOnType f = everywhereOnTypeM (f >>> Identity) >>> unwrap

-- | Subsitutte an existential everywhere inside a type
substitute :: Int -> Type -> Type -> Type
substitute id with = everywhereOnType case _ of
    Existential e | e.id == id -> with
    ty -> ty

-- | Substitute an universal everywhere inside a type
substituteUniversal :: String -> Type -> Type -> Type
substituteUniversal target with = case _ of
    Function from to -> 
        Function
            (substituteUniversal target with from) 
            (substituteUniversal target with to)
    Universal name | name == target -> with
    Forall name ty -> 
        Forall name if name == target 
            then ty 
            else substituteUniversal target with ty
    ty -> ty

occurs :: Existential -> Type -> Boolean
occurs target = go 
    where
    go = case _ of 
        Function from to -> go from || go to
        Forall _ ty -> go ty
        Existential existential -> target.id == existential.id
        _ -> false

--------- Typeclass instances
derive instance eqType :: Eq Type
derive instance genericType :: Generic Type _
instance debugType :: Debug Type where
  debug ty = genericDebug ty