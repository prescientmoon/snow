module Snow.Check where

import Prelude

import Array (insertManyBefore)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Run.Except (throw)
import Run.State (get, gets, modify)
import Run.Supply (generate)
import Snow.Context (CheckLogDetails(..), CheckM, ContextElement(..), InstantiationRule, beforeExistential, boundBefore, ensureWellFormed, getExistentialTypeM, getVariableTypeM, less, makeExistential, more, printContext, scopeMany, scoped, solve, zonk)
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

-- | Infer the otuput type for applying an argument e to function of type F
inferCall :: forall r. SnowType -> SnowType -> CheckM r SnowType
inferCall function argument = do
  get >>= \ctx -> Logger.log Logger.Debug (ctx /\ InferringCall argument function)
  result <- case function, argument of
    Forall name domain codomain, expr -> do
      existential <- makeExistential name
      scoped (CExistential existential domain Nothing) do
        inferCall
          (substituteUniversal name (Unsolved existential) codomain)
          expr
    Exists name domain codomain, expr -> do
      scoped (CUniversal name domain) do
        -- If this was not here,
        -- the following example would compile
        --  f everything
        --  where
        --  f :: exists something. something -> something
        --  everything :: forall a. a
        -- ensureWellFormed ctx'' ty'
        inferCall codomain argument
    Unsolved existential, expr -> do
      exLeft <- makeExistential $ existential.name <> "-left"
      exRight <- makeExistential $ existential.name <> "-right"
      modify $ insertManyBefore
        [ CExistential exRight (Star 0) Nothing
        , CExistential exLeft (Star 0) Nothing
        ]
        (CExistential existential (Star 0) Nothing)
      name <- generate <#> \id -> "_" <> show id
      solve existential (on (Pi name) Unsolved exLeft exRight)
      check argument $ Unsolved exLeft
      pure $ Unsolved exRight
    Pi name domain codomain, argument -> do
      check argument domain
      pure $ substituteUniversal name argument codomain

    _, _ -> throw "Illegal application"
  get >>= \ctx -> Logger.log Logger.Debug (ctx /\ InferredCall argument function result)
  pure result

-- | Make sure an expression has a type
check :: forall r. SnowType -> SnowType -> CheckM r Unit
check expr ty = do
  get >>= \ctx -> Logger.log Logger.Debug (ctx /\ Checking expr ty)
  case expr, ty of
    Forall name domain codomain, Star 0 -> do
      check domain (Star 0)
      scoped (CUniversal name domain) do
        check codomain (Star 0)
    Exists name domain codomain, Star 0 -> do
      check domain (Star 0)
      scoped (CUniversal name domain) do
        check codomain (Star 0)
    Pi name domain codomain, Star 0 -> do
      check domain (Star 0)
      scoped (CUniversal name domain) do
        check codomain (Star 0)
    Unit, Star 0 -> pure unit
    Star level, Star level'
      | level + 1 == level' -> pure unit
      | otherwise -> throw $ "Missmatched level " <> show level <> " " <> show level'
    ExprUnit, Unit -> pure unit
    expr, Forall name domain codomain -> do
      scoped (CUniversal name domain) do
        check expr codomain
    expr, Exists name domain codomain -> do
      existential <- makeExistential name
      scopeMany [ CMarker existential, CExistential existential domain Nothing ] do
        check expr $ substituteUniversal name (Unsolved existential) ty
    Lambda name body, Pi name' domain codomain -> do
      scopeMany [ CUniversal name domain, CUniversal name' domain ] do
        check body codomain
    expr, ty -> do
      inferred <- infer expr
      inferred <- zonk inferred
      ty <- zonk ty
      subtype inferred ty

infer :: forall r. SnowType -> CheckM r SnowType
infer ty = case ty of
  Universal name -> getVariableTypeM name >>= case _ of
    Just ty -> pure ty
    Nothing -> throw $ "Variable " <> name <> " not in scope"
  ExprUnit -> pure Unit
  Unit -> pure (Star 0)
  Star level -> pure (Star $ level + 1)
  Forall _ _ _ -> ado
    check ty (Star 0)
    in Star 0
  Pi _ _ _ -> ado
    check ty (Star 0)
    in Star 0
  Exists _ _ _ -> ado
    check ty (Star 0)
    in Star 0
  Annotation expr ty -> do
    get >>= \ctx -> ensureWellFormed ctx ty
    check expr ty
    pure ty
  Lambda name body -> do
    exLeft <- makeExistential $ name <> "-from"
    exRight <- makeExistential $ name <> "-to"
    let declaration = CUniversal name (Unsolved exLeft)
    modify \ctx -> ctx <>
      [ CExistential exLeft (Star 0) Nothing
      , CExistential exRight (Star 0) Nothing
      ]
    scoped (CUniversal name (Unsolved exLeft)) do
      check body (Unsolved exRight)
      pure $ on (Pi name) Unsolved exLeft exRight
  Application function argument -> do
    tyFunction <- infer function
    tyFunction <- zonk tyFunction
    inferCall tyFunction argument
  Effectful effect ty -> do
    -- TODO: check effects as well
    check ty (Star 0)
    pure (Star 0)
  Unsolved existential -> getExistentialTypeM existential >>= case _ of
    Just ty -> pure ty
    _ -> throw $ "Existential " <> existential.name <> " has escaped it's scope"