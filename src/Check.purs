module Snow.Check where

import Prelude

import Array (insertManyBefore)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Run.Except (throw)
import Run.State (get, gets, modify)
import Snow.Context (CheckLogDetails(..), CheckM, ContextElement(..), InstantiationRule, beforeExistential, boundBefore, ensureWellFormed, less, makeExistential, more, printContext, scopeMany, scoped, solve, zonk)
import Snow.Run.Logger as Logger
import Snow.Type (Existential, SnowType(..), occurs, substituteUniversal)

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

-- | Check a type is at least as general as another.
subtype :: forall r. SnowType -> SnowType -> CheckM r Unit
subtype left right = do
  get >>= \ctx -> Logger.log Logger.Debug (ctx /\ Subtyping left right)
  case left, right of
    -- forall-left
    Forall name domain codomain,
    right -> do
      existential <- makeExistential name
      let codomain' = substituteUniversal name (Unsolved existential) codomain
      scopeMany [ CMarker existential, CExistential existential domain Nothing ] do
        subtype codomain' right
    -- forall-right
    left,
    Forall name domain codomain ->
      scoped (CUniversal name domain) do
        subtype left codomain
    -- exists-left
    Exists _ _ _,
    right -> throw "unimplemented"
    left, Exists _ _ _ -> throw "unimplemented"
    -- functions
    Pi nameLeft fromLeft toLeft,
    Pi nameRight fromRight toRight -> do
      subtype fromRight fromLeft
      existential <- makeExistential nameLeft
      scopeMany [ CUniversal nameRight fromRight, CMarker existential, CExistential existential fromLeft Nothing ] do
        toLeft <- zonk $ substituteUniversal nameLeft (Unsolved existential) toLeft
        toRight <- zonk toRight
        ctx <- get
        traceM $ printContext ctx
        subtype toLeft toRight
    -- Equality cases
    Universal a,
    Universal b | a == b -> do
      context <- get
      ensureWellFormed context $ Universal a
    Unsolved a, Unsolved b | a == b -> pure unit
    Unit, Unit -> pure unit
    Star a, Star b
      | a == b -> pure unit
      | otherwise -> throw $ "Different * levels: " <> show a <> " and " <> show b
    -- TODO: better error messages for circular types
    -- Instantiation cases

    Unsolved existential,
    right | not (occurs existential right) -> instantiate less existential right
    left, Unsolved existential | not (occurs existential left) -> instantiate more existential left
    -- Failure
    t1,
    t2 -> throw $ "Unsolvable subtyping relation between " <> show t1 <> " and " <> show t2
