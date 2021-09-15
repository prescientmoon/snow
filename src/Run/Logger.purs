module Snow.Run.Logger where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Run (Run)
import Run.Writer (WRITER, tell)

---------- Types
data LogLevel = Debug | Dev | Prod
type Log l = LogLevel /\ l

type LOGGER l r = WRITER (Array (Log l)) r

---------- Helpers
log :: forall l r. LogLevel -> l -> Run (LOGGER l r) Unit
log level message = tell [ level /\ message ]

withLog :: forall r l. LogLevel -> l -> Run (LOGGER l r) ~> Run (LOGGER l r)
withLog level message computation = log level message *> computation

---------- Typeclass instances
derive instance Eq LogLevel