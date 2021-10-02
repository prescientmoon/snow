module Snow.Check where

import Prelude

import Array (insertManyBefore)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Run.Except (throw)
import Run.State (get, gets, modify)
import Snow.Context (CheckLogDetails(..), CheckM, ContextElement(..), InstantiationRule, beforeExistential, boundBefore, ensureWellFormed, less, makeExistential, more, scopeMany, scoped, solve, zonk)
import Snow.Run.Logger as Logger
import Snow.Type (Existential, SnowType(..), substituteUniversal)

-- | Instantiate an existential such that it's either less or more general than a type
instantiate :: forall r. InstantiationRule -> Existential -> SnowType -> CheckM r Unit
instantiate rule existential ty = do
  let
    trivial solution = do
      ctx <- gets (beforeExistential existential)
      ensureWellFormed ctx solution
      solve existential solution
  case ty of
    Unsolved other -> do
      boundBefore other existential
      solve other (Unsolved existential)
    Pi name domain codomain -> do
      exFrom <- makeExistential (existential.name <> "-left")
      exTo <- makeExistential (existential.name <> "-right")
      get >>= \ctx -> do
        modify $ insertManyBefore
          [ CExistential exFrom (Star 0) Nothing, CExistential exTo (Star 0) Nothing ]
          (CExistential existential (Star 0) Nothing)
        solve
          existential
          (on (Pi name) Unsolved exFrom exTo)
      instantiate (not rule) exFrom domain
      codomain <- zonk codomain
      scoped (CUniversal name domain) do
        instantiate rule exTo codomain
    Forall name domain ty
      | rule == less -> scoped (CUniversal name domain) do
          instantiate less existential ty
      | rule == more -> do
          exForall <- makeExistential name
          let roTy = substituteUniversal name (Unsolved exForall) ty
          scopeMany [ CMarker exForall, CExistential exForall domain Nothing ] do
            instantiate more existential roTy
      | otherwise -> throw "Unreachable"
    Exists name domain ty -> {- TODO: test this -} do
      instantiate (not rule) existential (Forall name domain ty)

    Universal _ -> trivial ty
    ExprUnit -> trivial ty
    Unit -> trivial ty
    Star _ -> trivial ty
    Effectful _ _ -> throw "Unimplemented"
    Annotation _ _ -> throw "Unimplemented"
    Application _ _ -> throw "Unimplemented"
    Lambda _ _ -> throw "Unimplemented"

  -- LOGGING:
  get >>= \res -> Logger.log Logger.Debug $ res /\ Instantiating rule existential ty
