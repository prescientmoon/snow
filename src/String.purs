module Snow.Stinrg where

import Prelude

indent :: Int -> String -> String
indent n a
  | n <= 0 = a
  | otherwise = " " <> indent (n - 1) a