module Snow.Repl where

import Prelude

import Context (infer_, subtype_)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ReadLine.Aff (Interface, prompt)
import Parser (parseCommand)
import Snow.Debug (showPretty)
import Snow.Repl.Types (Command(..))
import Snow.Type (printType)
import Text.Parsing.Parser (parseErrorMessage)

type ReplState =
  { interface :: Interface }

-- | Interpreting of commaneds
interpret :: Command -> Effect Unit
interpret (TypeOf expression) = do
  log $ printType type_
  log $ showPretty context
  where
  context /\ type_ = infer_ expression
interpret (Subsumes a b) = do
  log $ showPretty context
  where
  context = subtype_ a b

loop :: ReplState -> Aff Unit
loop state = do
  input <- prompt state.interface
  case parseCommand input of
    Left error -> log $ parseErrorMessage error
    Right command -> liftEffect $ interpret command
  loop state

