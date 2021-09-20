module Snow.Stinrg where

import Prelude

import Data.String (Pattern(..), joinWith, split)

-- | Indent a multi line string
indent :: Int -> String -> String
indent amount = split (Pattern "\n") >>> map (indentOne amount) >>> joinWith "\n"

-- | Indents a single line string
indentOne :: Int -> String -> String
indentOne n a
  | n <= 0 = a
  | otherwise = " " <> indentOne (n - 1) a