module Snow.Repl where

import Prelude

import Context (infer, runCheckMWithConsole)
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
import Snow.Run.Logger (LogLevel(..))
import Snow.Type (printType)
import Text.Parsing.Parser (parseErrorMessage)

type ReplState =
  { interface :: Interface }

-- | Interpreting of commaneds
interpret :: Command -> Effect Unit
interpret (TypeOf expression) = do
  runCheckMWithConsole Debug
    ( \(context /\ type_) -> do
        log $ printType type_
        log $ showPretty context
    )
    (infer [] expression)
interpret (Subsumes a b) = do
  log "Unimplemented"

loop :: ReplState -> Aff Unit
loop state = do
  input <- prompt state.interface
  case parseCommand input of
    Left error -> log $ parseErrorMessage error
    Right command -> liftEffect $ interpret command
  loop state

