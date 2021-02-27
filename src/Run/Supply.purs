module Run.Supply where

import Prelude
import Data.Tuple (Tuple(..))
import Run (FProxy, Run, SProxy(..), Step(..), lift, on, runAccumPure)

-- | Monad providing an infinite supply of values of a particular type.
-- | Example use cases: generating unique ids.
data SupplyF s a = Supply (s -> a)

type SUPPLY s = FProxy (SupplyF s)

generate :: forall r s. Run ( supply :: SUPPLY s | r ) s
generate = lift _supply (Supply identity)

-- | Elimininate the supply monad using a function generating the next value
runSupply :: forall r s a. (s -> s) -> s -> Run ( supply :: SUPPLY s | r ) a -> Run r a
runSupply next 
    = runAccumPure 
        (next >>> \current -> on _supply (Loop <<< handleSupply current) Done)
        (\s a -> a)
    where
    handleSupply :: forall i. s -> SupplyF s i -> Tuple s i
    handleSupply current (Supply continue) = Tuple current (continue current)

---------- Typeclass instances
derive instance functorSupply :: Functor (SupplyF s)

--------- SProxies
_supply :: SProxy "supply"
_supply = SProxy