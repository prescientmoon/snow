module Snow.Repl where

import Prelude

import Context (Context, ContextElement(..), infer, isWellFormed, runCheckMWithConsole)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.ReadLine.Aff (Interface, prompt)
import Parser (parseCommand)
import Run (AFF, EFFECT, Run, liftEffect, runBaseAff')
import Run.Reader (READER, asks, runReader)
import Run.State (STATE, evalState, gets, modify)
import Snow.Repl.Types (Command(..))
import Snow.Run.Logger (LogLevel(..))
import Snow.Stinrg (indent)
import Text.Parsing.Parser (parseErrorMessage)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type ReplEnv =
  { interface :: Interface
  }

type ReplState =
  { context :: Context
  }

-- | Interpreting of commaneds
interpret :: forall r. Command -> Run (EFFECT + READER ReplEnv + STATE ReplState r) Unit
interpret (TypeOf expression) = do
  context <- gets _.context
  liftEffect $ runCheckMWithConsole Debug
    ( \(context /\ type_) -> do
        log $ joinWith "\n"
          [ "Inferred type"
          , indent 4 $ show type_
          , "in context"
          , indent 4 $ joinWith "\n" $ map show context
          ]
    )
    (infer context expression)
interpret (Subsumes a b) = do
  log "Unimplemented"
interpret (Assume name type_) = do
  context <- gets _.context
  if isWellFormed context type_ then
    modify (over _context $ flip Array.snoc $ CDeclaration name type_)
  else log $ joinWith "\n"
    [ "Type"
    , indent 4 $ show type_
    , "is not well formed in context"
    , indent 4 $ show context
    ]

loop :: forall r. Run (EFFECT + AFF + READER ReplEnv + STATE ReplState r) Unit
loop = do
  interface <- asks _.interface
  input <- prompt interface
  case parseCommand input of
    Left error -> log $ parseErrorMessage error
    Right command -> interpret command
  loop

runRepl :: ReplEnv -> Run (EFFECT + AFF + READER ReplEnv + STATE ReplState ()) ~> Aff
runRepl env = runReader env >>> evalState mempty >>> runBaseAff'

---------- Lenses
_context :: Lens' ReplState Context
_context = prop (Proxy :: _ "context")