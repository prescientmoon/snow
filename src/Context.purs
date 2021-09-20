module Context where

import Prelude

import Array (insertManyBefore)
import Data.Array (any, filter, snoc, takeWhile)
import Data.Debug (class Debug, genericDebug)
import Data.Either (Either(..))
import Data.Foldable (findMap, for_)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Run (Run, extract)
import Run.Except (EXCEPT, runExcept, throw)
import Run.Supply (SUPPLY, generate, runSupply)
import Run.Writer (runWriter)
import Snow.Ast (Expr(..))
import Snow.Debug (showPretty)
import Snow.Run.Logger (LOGGER, LogLevel(..))
import Snow.Run.Logger as Logger
import Snow.Stinrg (indent)
import Snow.Type (Existential, SnowType(..), everywhereOnType, occurs, substituteUniversal)
import Type.Row (type (+))

data ContextElement
  = CUniversal String
  | CExistential Existential (Maybe SnowType)
  | CDeclaration String SnowType
  | CMarker Existential

type Context = Array ContextElement

data CheckLogDetails
  = Checking Expr SnowType
  | Inferring Expr
  | Inferred Expr SnowType
  | InferringCall Expr SnowType
  | InferredCall Expr SnowType SnowType
  | Instantiating InstantiationRule Existential SnowType
  | Subtyping SnowType SnowType
  | Solved Existential SnowType

type CheckLog = Context /\ CheckLogDetails

type CheckM r = Run (LOGGER CheckLog + SUPPLY Int + EXCEPT String r)

--------- Helpers
printContext :: Context -> String
printContext = joinWith "\n" <<< map show

isWellFormed :: Context -> SnowType -> Boolean
isWellFormed ctx (Function from to) = isWellFormed ctx from && isWellFormed ctx to
isWellFormed ctx (Forall var ty) = isWellFormed (snoc ctx (CUniversal var)) ty
isWellFormed ctx (Exists var ty) = isWellFormed (snoc ctx (CUniversal var)) ty
isWellFormed ctx (Universal universal) = any ((==) (CUniversal universal)) ctx
isWellFormed ctx (Existential { id }) = ctx # any case _ of
  CExistential current _ | current.id == id -> true
  _ -> false
isWellFormed ctx Unit = true

getVariable :: String -> Context -> Maybe SnowType
getVariable target = findMap case _ of
  CDeclaration name ty | name == target -> Just ty
  _ -> Nothing

ensureWellFormed :: forall r. Context -> SnowType -> Run (EXCEPT String r) Unit
ensureWellFormed context type_ = do
  unless (isWellFormed context type_) do
    throw $ joinWith "\n"
      [ "A type variable probably escaped it's scope. Type"
      , indent 4 $ show type_
      , "is not well formed in context"
      , indent 4 $ printContext context
      ]

solve :: forall r. Existential -> SnowType -> Context -> Run (EXCEPT String + LOGGER CheckLog r) Context
solve target solution ctx = ctx # traverseWithIndex case _, _ of
  index, element@(CExistential existential Nothing)
    | existential.id == target.id -> do
        ensureWellFormed (beforeElement element ctx) solution
        Logger.log Debug (ctx /\ Solved existential solution)
        pure $ CExistential existential $ Just solution
  _, e -> pure e

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
makeExistential :: forall r. String -> CheckM r Existential
makeExistential name = generate <#> { name, id: _ }

--------- SnowType checking & friends
-- | Check a type is at least as general as another.
subtype :: forall r. Context -> SnowType -> SnowType -> CheckM r Context
subtype ctx left right = do
  Logger.log Debug (ctx /\ Subtyping left right)
  go left right
  where
  go = flip on (applyContext ctx) case _, _ of
    -- forall-left
    Forall name left,
    right -> do
      existential <- makeExistential name
      let ctx' = ctx <> [ CMarker existential, CExistential existential Nothing ]
      let left' = substituteUniversal name (Existential existential) left
      beforeMarker existential <$> subtype ctx' left' right
    -- exists-left
    Exists name left,
    right -> throw "unimplemented"
    left, Exists name right -> throw "unimplemented"
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
    t2 -> throw $ "Unsolvable subtyping relation :( =>" <> showPretty { t1, t2 }

--------- Instantiation
type InstantiationRule = Boolean

less :: InstantiationRule
less = false

more :: InstantiationRule
more = true

-- | Instantiate an existential such that it's either less or more general than a type
instantiate :: forall r. InstantiationRule -> Context -> Existential -> SnowType -> CheckM r Context
instantiate rule ctx existential = applyContext ctx >>> go
  where
  go ty = do
    res <- go' ty
    Logger.log Debug $ res /\ Instantiating rule existential ty
    pure res
  go' = case _ of
    Existential other | boundBefore other existential ctx ->
      solve other (Existential existential) ctx
    Function from to -> do
      exFrom <- makeExistential (existential.name <> "-left")
      exTo <- makeExistential (existential.name <> "-right")
      ctx' <-
        insertManyBefore
          [ CExistential exTo Nothing, CExistential exFrom Nothing ]
          (CExistential existential Nothing)
          ctx
          # solve
            existential
            (on Function Existential exFrom exTo)
      ctx'' <- instantiate (not rule) ctx' exFrom from
      instantiate rule ctx'' exTo to
    Exists name ty -> throw "Unimplemented"
    Forall name ty
      | rule == less -> beforeUniversal name <$> instantiate less (snoc ctx $ CUniversal name) existential ty
      | rule == more -> do
          exForall <- makeExistential name
          let roTy = substituteUniversal name (Existential exForall) ty
          let ctx' = ctx <> [ CMarker exForall, CExistential exForall Nothing ]
          beforeMarker exForall <$> instantiate more ctx' existential ty
    ty -> solve existential ty ctx

-- | Infer the otuput type for applying an argument e to function of type F
inferCall :: forall r. Context -> SnowType -> Expr -> CheckM r (Tuple Context SnowType)
inferCall ctx = wrapper case _, _ of
  Forall name ty, expr -> do
    existential <- makeExistential name
    inferCall
      (snoc ctx $ CExistential existential Nothing)
      (substituteUniversal name (Existential existential) ty)
      expr
  Exists name ty, expr -> do
    ctx' /\ ty' <- inferCall
      (snoc ctx $ CUniversal name)
      ty
      expr
    let ctx'' = beforeUniversal name ctx'
    -- If this was not here,
    -- the following example would compile
    --  f everything
    --  where
    --  f :: exists something. something -> something
    --  everything :: forall a. a
    ensureWellFormed ctx'' ty'
    pure $ ctx'' /\ ty'
  Existential existential, expr -> do
    exLeft <- makeExistential $ existential.name <> "-left"
    exRight <- makeExistential $ existential.name <> "-right"
    ctx' <- ctx
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
  _, _ -> throw "Illegal application"
  where
  wrapper f ty expr = do
    Logger.log Debug (ctx /\ InferringCall expr ty)
    ctx' /\ to <- go <$> f (applyContext ctx ty) expr
    Logger.log Debug (ctx /\ InferredCall expr ty to)
    pure $ ctx' /\ to
    where
    go (Tuple ctx' ty') = Tuple ctx' $ applyContext ctx' ty'

-- | Synthesise the type of an expression
infer :: forall r. Context -> Expr -> CheckM r (Tuple Context SnowType)
infer ctx = zonk case _ of
  ExprVariable name -> case getVariable name ctx of
    Just ty -> pure $ Tuple ctx ty
    Nothing -> throw "Variable not in scope"
  ExprUnit -> pure $ Tuple ctx Unit
  ExprAnnotation expr ty -> do
    -- TODO: check A is well formed
    ctx' <- check ctx expr ty
    pure $ Tuple ctx' ty
  ExprLambda name body -> do
    exLeft <- makeExistential $ name <> "-from"
    exRight <- makeExistential $ name <> "-to"
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
  zonk f e = do
    Logger.log Debug (ctx /\ Inferring e)
    ctx' /\ ty <- go <$> f e
    Logger.log Debug (ctx' /\ Inferred e ty)
    pure $ ctx' /\ ty
    where
    go (Tuple ctx' ty) = Tuple ctx' $ applyContext ctx' ty

-- | Make sure an expression has a type
check :: forall r. Context -> Expr -> SnowType -> CheckM r Context
check ctx = wrapper case _, _ of
  ExprUnit, Unit -> pure ctx
  expr, Forall name ty -> do
    let ctx' = snoc ctx (CUniversal name)
    ctx'' <- check ctx' expr ty
    pure $ beforeUniversal name ctx''
  expr, Exists name ty -> do
    existential <- makeExistential name
    let ctx' = ctx <> [ CMarker existential, CExistential existential Nothing ]
    ctx'' <- check ctx' expr $ substituteUniversal name (Existential existential) ty
    pure $ beforeMarker existential ctx''
  ExprLambda name body, Function from to -> do
    let declaration = CDeclaration name from
    ctx' <- check (snoc ctx declaration) body to
    pure $ beforeElement declaration ctx'
  expr, ty -> do
    Tuple ctx' inferred <- infer ctx expr
    subtype ctx' inferred ty
  where
  wrapper f expr ty = do
    Logger.log Debug (ctx /\ Checking expr ty)
    f expr $ applyContext ctx ty

--------- Debugging
infer_ :: Expr -> Either String (Array (LogLevel /\ Array ContextElement /\ CheckLogDetails) /\ Context /\ SnowType)
infer_ e = extract $ runExcept $ runWriter $ runSupply ((+) 1) 0 $ infer [] e

runCheckMWithConsole :: forall a. LogLevel -> (a -> Effect Unit) -> CheckM () a -> Effect Unit
runCheckMWithConsole level callback computation = do
  let logs /\ result = extract $ runWriter $ runExcept $ runSupply ((+) 1) 0 computation
  log "LOGS:"
  for_ (filter (fst >>> ((==) level)) logs <#> snd) \(ctx /\ message) -> do
    log "\n=========="
    logShow message
    log "----------"
    for_ ctx \e -> logShow e
  case result of
    Left err -> log err
    Right success -> do
      callback success

--------- Typeclass instances
derive instance eqCE :: Eq ContextElement
derive instance genericContextElement :: Generic ContextElement _
instance debugContextElement :: Debug ContextElement where
  debug = genericDebug
instance Show ContextElement where
  show (CExistential { name } ty) = case ty of
    Nothing -> "?" <> name
    Just ty -> "?" <> name <> " = " <> show ty
  show (CDeclaration name ty) = name <> " :: " <> show ty
  show (CUniversal uni) = uni
  show (CMarker { name }) = ">>> " <> name

derive instance genericCheckLogDetails :: Generic CheckLogDetails _
instance Debug CheckLogDetails where
  debug = genericDebug
instance Show CheckLogDetails where
  show (Checking expr ty) = joinWith "\n"
    [ "Checking that expression"
    , indent 4 $ show expr
    , "has type"
    , indent 4 $ show ty
    ]
  show (Inferring expr) = joinWith "\n"
    [ "Inferring the type of expression"
    , indent 4 $ show expr
    ]
  show (Inferred expr type_) = joinWith "\n"
    [ "Inferred the expression"
    , indent 4 $ show expr
    , "to have type"
    , indent 4 $ show type_
    ]
  show (InferringCall expr ty) = joinWith "\n"
    [ "Inferring the result of applying a function of type"
    , indent 4 $ show ty
    , "to the argument"
    , indent 4 $ show expr
    ]
  show (InferredCall expr ty to) = joinWith "\n"
    [ "The result of applying a function of type"
    , indent 4 $ show ty
    , "to the argument"
    , indent 4 $ show expr
    , "has type"
    , indent 4 $ show to
    ]
  show (Subtyping left right) = joinWith "\n"
    [ "Checking that type"
    , indent 4 $ show left
    , "is less general than type"
    , indent 4 $ show right
    ]
  show (Instantiating rule { name } right) = joinWith "\n"
    [ "Instantiating existential"
    , indent 4 name
    , "so it is " <> (if rule == more then "more" else "less") <> " general than type"
    , indent 4 $ show right
    ]
  show (Solved { name } to) = joinWith "\n"
    [ "Solving existential"
    , indent 4 $ "?" <> name
    , "to type"
    , indent 4 $ show to
    ]
