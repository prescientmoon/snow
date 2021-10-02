module Snow.Context where

import Prelude

import Data.Array (any, head, snoc, takeWhile)
import Data.Debug (class Debug, constructor, genericDebug)
import Data.Either (Either(..))
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Run (Run, extract)
import Run.Except (EXCEPT, runExcept, throw)
import Run.State (STATE, evalState, get, modify, put)
import Run.Supply (SUPPLY, generate, runSupply)
import Run.Writer (runWriter)
import Snow.Run.Logger (LOGGER, LogLevel(..))
import Snow.Run.Logger as Logger
import Snow.Stinrg (indent)
import Snow.Type (SnowType(..), Existential, everywhereOnType)
import Type.Row (type (+))

data ContextElement
  = CUniversal String SnowType
  | CExistential Existential SnowType (Maybe SnowType)
  | CMarker Existential

type Context = Array ContextElement

data CheckLogDetails
  = Checking SnowType SnowType
  | Inferring SnowType
  | Inferred SnowType SnowType
  | InferringCall SnowType SnowType
  | InferredCall SnowType SnowType SnowType
  | Instantiating InstantiationRule Existential SnowType
  | Subtyping SnowType SnowType
  | Solved Existential SnowType

type CheckLog = Context /\ CheckLogDetails

type CheckM r = Run (LOGGER CheckLog + SUPPLY Int + STATE Context + EXCEPT String r)

--------- Instantiation
newtype InstantiationRule = InstantiationRule Boolean

less :: InstantiationRule
less = InstantiationRule false

more :: InstantiationRule
more = InstantiationRule true

---------- State helpers
mutateContext :: forall r. (Context -> CheckM r Context) -> CheckM r Unit
mutateContext f = get >>= f >>= put

--------- Helpers
printContext :: Context -> String
printContext = joinWith "\n" <<< map show

isWellFormed :: Context -> SnowType -> Boolean
isWellFormed ctx (Pi name domain codomain) = isWellFormed ctx domain
  && isWellFormed (snoc ctx $ CUniversal name domain) codomain
isWellFormed ctx (Forall name domain codomain) = isWellFormed ctx domain
  && isWellFormed (snoc ctx (CUniversal name domain)) codomain
isWellFormed ctx (Exists name domain codomain) = isWellFormed ctx domain
  && isWellFormed (snoc ctx (CUniversal name domain)) codomain
isWellFormed ctx (Universal target) = ctx # any case _ of
  CUniversal name _ -> name == target
  _ -> false
isWellFormed ctx (Unsolved { id }) = ctx # any case _ of
  CExistential current _ _ | current.id == id -> true
  _ -> false
isWellFormed ctx (Application left right) = isWellFormed ctx left && isWellFormed ctx right
isWellFormed ctx (Annotation expr annotation) = isWellFormed ctx expr && isWellFormed ctx annotation
isWellFormed ctx (Effectful effect ty) = isWellFormed ctx effect && isWellFormed ctx ty
isWellFormed ctx (Lambda argument body) = isWellFormed (snoc ctx (CUniversal argument {- TODO: ensure this never gets used -} Unit)) body
isWellFormed ctx (Star _) = true
isWellFormed ctx Unit = true
isWellFormed ctx ExprUnit = true

getVariableType :: String -> Context -> Maybe SnowType
getVariableType target = findMap case _ of
  CUniversal name ty | name == target -> Just ty
  _ -> Nothing

-- | Monadic version of `getVariableType`
getVariableTypeM :: forall r. String -> CheckM r (Maybe SnowType)
getVariableTypeM name = get <#> getVariableType name

getExistentialType :: Existential -> Context -> Maybe SnowType
getExistentialType { id: target } = findMap case _ of
  CExistential { id } ty _ | id == target -> Just ty
  _ -> Nothing

-- | Monadic version of `getExistentialType`
getExistentialTypeM :: forall r. Existential -> CheckM r (Maybe SnowType)
getExistentialTypeM name = get <#> getExistentialType name

ensureWellFormed :: forall r. Context -> SnowType -> CheckM r Unit
ensureWellFormed context type_ = do
  unless (isWellFormed context type_) do
    throw $ joinWith "\n"
      [ "A type variable probably escaped it's scope. Type"
      , indent 4 $ show type_
      , "is not well formed in context"
      , indent 4 $ printContext context
      ]

solve :: forall r. Existential -> SnowType -> CheckM r Unit
solve target solution = mutateContext \ctx ->
  ctx # traverseWithIndex case _, _ of
    index, element@(CExistential existential domain solved)
      | isJust solved -> throw $ "Existential " <> target.name <> " has already been solved"
      | existential.id == target.id -> do
          ensureWellFormed (beforeElement element ctx) solution
          Logger.log Debug (ctx /\ Solved existential solution)
          pure $ CExistential existential domain $ Just solution
    _, e -> pure e

-- | Get the solution for an existential
getSolution :: Context -> Existential -> Maybe SnowType
getSolution ctx e = join $ findMap go ctx
  where
  go (CExistential e' domain solution) | e == e' = Just (solution)
  go _ = Nothing

-- | Replace all existentials in a type with their solutions
applyContext :: Context -> SnowType -> SnowType
applyContext ctx = everywhereOnType case _ of
  Unsolved e -> case getSolution ctx e of
    Just solution -> applyContext ctx solution
    Nothing -> Unsolved e
  ty -> ty

-- | Returns true if the second param was bound before the first
boundBefore :: forall r. Existential -> Existential -> CheckM r Unit
boundBefore first second = get <#> findMap found >>= case _ of
  Just true -> pure unit
  _ -> throw $ "Variable " <> first.name <> " must be bound before " <> second.name
  where
  found = case _ of
    CExistential e domain _
      | e == first -> Just false
      | e == second -> Just true
    _ -> Nothing

-- | Take all the elements of a context which appear before an arbitrary universal.
beforeUniversal :: String -> Context -> Context
beforeUniversal target = takeWhile case _ of
  CUniversal name _ -> name /= target
  _ -> true

beforeMarker :: Existential -> Context -> Context
beforeMarker target = takeWhile \a -> a /= CMarker target

beforeExistential :: Existential -> Context -> Context
beforeExistential { id: target } = takeWhile case _ of
  CExistential { id } _ _ -> id /= target
  _ -> true

beforeElement :: ContextElement -> Context -> Context
beforeElement target = takeWhile \a -> a /= target

-- | Make an existential with an unique id
makeExistential :: forall r. String -> CheckM r Existential
makeExistential name = generate <#> { name, id: _ }

-- | Scope a computation by promising to delete all the elements past
-- | a provided point after the computations has been run
scoped :: forall r. ContextElement -> CheckM r ~> CheckM r
scoped element computation = do
  modify (flip snoc element)
  computation <* modify (beforeElement element)

-- | Same as `scoped` but works for more than 1 element at a time
scopeMany :: forall r. Array ContextElement -> CheckM r ~> CheckM r
scopeMany elements computation
  | Just element <- head elements = do
      modify (\ctx -> ctx <> elements)
      result <- computation
      modify (beforeElement element)
      pure result
  | otherwise = computation

-- | Apply the current context onto a type
zonk :: forall r. SnowType -> CheckM r SnowType
zonk ty = get <#> flip applyContext ty

---------- Running checks
runCheckMWithConsole :: forall a. Show a => Context -> CheckM () a -> Effect Unit
runCheckMWithConsole context computation = do
  let logs /\ result = extract $ evalState context $ runWriter $ runExcept $ runSupply ((+) 1) 0 computation
  case result of
    Left err -> log err
    Right success -> do
      logShow success

---------- Typeclass instances
derive instance Eq InstantiationRule
derive instance Generic InstantiationRule _
instance Debug InstantiationRule where
  debug rule = if rule == less then constructor "Less" [] else constructor "More" []

derive newtype instance HeytingAlgebra InstantiationRule

derive instance Eq ContextElement
derive instance Generic ContextElement _
instance Debug ContextElement where
  debug = genericDebug
instance Show ContextElement where
  show (CExistential { name } domain ty) = case ty of
    Nothing -> "?" <> (name <> " :: " <> show ty)
    Just ty -> "?" <> ("(" <> name <> " :: " <> show ty <> ")") <> " = " <> show ty
  show (CUniversal uni ty) = uni <> " :: " <> show ty
  show (CMarker { name }) = ">>> " <> name

derive instance Generic CheckLogDetails _
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