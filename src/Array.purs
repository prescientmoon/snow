module Array where

import Prelude

-- | Insert an array of elements before an arbitrary element inside another array
insertManyBefore :: forall a. Eq a => Array a -> a -> Array a -> Array a 
insertManyBefore = insertManyBeforeImpl eq

foreign import insertManyBeforeImpl :: forall a. (a -> a -> Boolean) -> Array a -> a -> Array a -> Array a 