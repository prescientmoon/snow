module Main where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class.Console (error)
import Node.ReadLine (createConsoleInterface, noCompletion)
import Node.ReadLine.Aff as RL
import Snow.Repl (loop, runRepl)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  runAff_
    ( either
        (\err -> showError err *> RL.close interface)
        (const $ RL.close interface)
    )
    (runRepl { interface } loop)
  where
  showError err = error (show err)