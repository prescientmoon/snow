module Context where

import Prelude

import Array (insertManyBefore)
import Data.Array (snoc, takeWhile)
import Data.Debug (class Debug, genericDebug)
import Data.Foldable (findMap)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Parser (unsafeParseType)
import Partial.Unsafe (unsafeCrashWith)
import Run (Run, extract, lift)
import Run.Supply (SUPPLY, SupplyF(..), _supply, generate, runSupply)
import Snow.Ast (Expr(..))
import Snow.Debug (showPretty)
import Snow.Type (Existential, SnowType(..), everywhereOnType, occurs, substituteUniversal)

data ContextElement
  = CUniversal String
  | CExistential Existential (Maybe SnowType)
  | CDeclaration String SnowType
  | CMarker Existential

type Context = Array ContextElement

--------- Helpers
getVariable :: String -> Context -> Maybe SnowType
getVariable target = findMap case _ of
  CDeclaration name ty | name == target -> Just ty
  _ -> Nothing

solve :: Existential -> SnowType -> Context -> Context
solve target solution = map case _ of
  CExistential existential Nothing
    | existential.id == target.id -> CExistential existential $ Just solution
  e -> e

-- | Get the solution for an existential
getSolution :: Context -> Existential -> Maybe SnowType
getSolution ctx e = join $ findMap go ctx
  where
  go (CExistential e' solution) | e == e' = Just (solution)
  go _ = Nothing

-- | Replace all existentials in a type with their solutions
applyContext :: Context -> SnowType -> SnowType
applyContext ctx = everywhereOnType case _ of
  Existential e -> case getSolution ctx e of
    Just solution -> applyContext ctx solution
    Nothing -> Existential e
  ty -> ty

-- | Returns true if the second param was bound before the first
boundBefore :: Existential -> Existential -> Context -> Boolean
boundBefore first second = fromMaybe false <<< findMap case _ of
  CExistential e _
    | e == first -> Just false
    | e == second -> Just true
  _ -> Nothing

-- | Take all the elements of a context which appear before an arbitrary universal.
beforeUniversal :: String -> Context -> Context
beforeUniversal target = takeWhile \a -> a /= CUniversal target

beforeMarker :: Existential -> Context -> Context
beforeMarker target = takeWhile \a -> a /= CMarker target

beforeElement :: ContextElement -> Context -> Context
beforeElement target = takeWhile \a -> a /= target

-- | Make an existential with an unique id
makeExistential :: forall r. String -> Run (SUPPLY Int r) Existential
makeExistential name = generate <#> { name, id: _ }

--------- SnowType checking & friends
-- | Check a type is at least as general as another.
subtype :: forall r. Context -> SnowType -> SnowType -> Run (SUPPLY Int r) Context
subtype ctx = flip on (applyContext ctx) case _, _ of
  -- forall-left
  Forall name left,
  right -> do
    existential <- makeExistential name
    let ctx' = ctx <> [ CMarker existential, CExistential existential Nothing ]
    let left' = substituteUniversal name (Existential existential) left
    beforeMarker existential <$> subtype ctx' left' right
  -- forall-right
  left,
  Forall name right ->
    beforeUniversal name <$> subtype ctx' left right
    where
    ctx' = snoc ctx (CUniversal name)
  -- functions
  Function fromLeft toLeft,
  Function fromRight toRight -> do
    ctx' <- subtype ctx fromRight fromLeft
    subtype ctx' (applyContext ctx' toLeft) (applyContext ctx' toRight)
  -- Equality cases
  Universal a,
  Universal b | a == b -> pure ctx
  Existential a, Existential b | a == b -> pure ctx
  Unit, Unit -> pure ctx
  -- Instantiation cases
  -- TODO: better error messages for circular types
  Existential existential,
  right | not (occurs existential right) -> instantiate less ctx existential right
  left, Existential existential | not (occurs existential left) -> instantiate more ctx existential left
  -- Failure
  t1,
  t2 -> lift _supply $ Supply \_ -> unsafeCrashWith $ "Unsolvable subtyping relation :( =>" <> showPretty { t1, t2 }

--------- Instantiation
type InstantiationRule = Boolean

less :: InstantiationRule
less = false

more :: InstantiationRule
more = true

-- | Instantiate an existential such that it's either less or more general than a type
instantiate :: forall r. InstantiationRule -> Context -> Existential -> SnowType -> Run (SUPPLY Int r) Context
instantiate rule ctx existential = applyContext ctx >>> case _ of
  Existential other | boundBefore other existential ctx ->
    pure $ solve other (Existential existential) ctx
  Function from to -> do
    exFrom <- makeExistential (existential.name <> "-left")
    exTo <- makeExistential (existential.name <> "-right")
    let
      ctx'
        =
        insertManyBefore
          [ CExistential exTo Nothing, CExistential exFrom Nothing ]
          (CExistential existential Nothing)
          ctx
          # solve
            existential
            (on Function Existential exFrom exTo)
    ctx'' <- instantiate (not rule) ctx' exFrom from
    instantiate rule ctx'' exTo to
  Forall name ty
    | rule == less -> beforeUniversal name <$> instantiate less (snoc ctx $ CUniversal name) existential ty
    | rule == more -> do
        exForall <- makeExistential name
        let roTy = substituteUniversal name (Existential exForall) ty
        let ctx' = ctx <> [ CMarker exForall, CExistential exForall Nothing ]
        beforeMarker exForall <$> instantiate more ctx' existential ty
  ty -> pure $ solve existential ty ctx

-- | Infer the otuput type for applying an argument e to function of type F
inferCall :: forall r. Context -> SnowType -> Expr -> Run (SUPPLY Int r) (Tuple Context SnowType)
inferCall ctx = wrapper case _, _ of
  Forall name ty, expr -> do
    existential <- makeExistential name
    inferCall
      (snoc ctx $ CExistential existential Nothing)
      (substituteUniversal name (Existential existential) ty)
      expr
  Existential existential, expr -> do
    exLeft <- makeExistential $ existential.name <> "-left"
    exRight <- makeExistential $ existential.name <> "-right"
    let
      ctx' = ctx
        # insertManyBefore
          [ CExistential exRight Nothing
          , CExistential exLeft Nothing
          ]
          (CExistential existential Nothing)
        # solve existential (on Function Existential exLeft exRight)
    ctx'' <- check ctx' expr $ Existential exLeft
    pure $ Tuple ctx'' $ Existential exRight
  Function left right, expr -> do
    ctx' <- check ctx expr left
    pure $ Tuple ctx' right
  _, _ -> unsafeCrashWith "Illegal application"
  where
  wrapper f ty expr = go <$> f (applyContext ctx ty) expr
    where
    go (Tuple ctx' ty') = Tuple ctx' $ applyContext ctx' ty'

-- | Synthesise the type of an expression
infer :: forall r. Context -> Expr -> Run (SUPPLY Int r) (Tuple Context SnowType)
infer ctx = zonk case _ of
  ExprVariable name -> case getVariable name ctx of
    Just ty -> pure $ Tuple ctx ty
    Nothing -> unsafeCrashWith "Variable not in scope"
  ExprUnit -> pure $ Tuple ctx Unit
  ExprAnnotation expr ty -> do
    -- TODO: check A is well formed
    ctx' <- check ctx expr ty
    pure $ Tuple ctx' ty
  ExprLambda name body -> do
    exLeft <- makeExistential "from"
    exRight <- makeExistential "to"
    let declaration = CDeclaration name (Existential exLeft)
    let
      ctx' = ctx <>
        [ CExistential exLeft Nothing
        , CExistential exRight Nothing
        , declaration
        ]
    ctx'' <- beforeElement declaration <$> check ctx' body (Existential exRight)
    pure $ Tuple ctx'' $ on Function Existential exLeft exRight
  ExprCall function argument -> do
    Tuple ctx' tyFunction <- infer ctx function
    inferCall ctx tyFunction argument
  where
  zonk f e = go <$> f e
    where
    go (Tuple ctx' ty) = Tuple ctx' $ applyContext ctx' ty

-- | Make sure an expression has a type
check :: forall r. Context -> Expr -> SnowType -> Run (SUPPLY Int r) Context
check ctx = wrapper case _, _ of
  ExprUnit, Unit -> pure ctx
  expr, Forall name ty -> do
    let ctx' = snoc ctx (CUniversal name)
    ctx'' <- check ctx' expr ty
    pure $ beforeUniversal name ctx''
  ExprLambda name body, Function from to -> do
    let declaration = CDeclaration name from
    ctx' <- check (snoc ctx declaration) body to
    pure $ beforeElement declaration ctx'
  expr, ty -> do
    Tuple ctx' inferred <- infer ctx expr
    subtype ctx' inferred ty
  where
  wrapper f expr ty = f expr $ applyContext ctx ty

--------- Debugging
unsafeSubtype :: String -> String -> Context
unsafeSubtype left right = extract $ runSupply ((+) 1) 0 $ subtype [] (unsafeParseType left) (unsafeParseType right)

infer_ :: Expr -> Tuple Context SnowType
infer_ e = extract $ runSupply ((+) 1) 0 $ infer [] e

subtype_ :: SnowType -> SnowType -> Context
subtype_ left right = extract $ runSupply ((+) 1) 0 $ subtype [] left right

--------- Typeclass instances
derive instance eqCE :: Eq ContextElement
derive instance genericContextElement :: Generic ContextElement _
instance debugContextElement :: Debug ContextElement where
  debug = genericDebug