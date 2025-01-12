module Andamio.Index.OnChain.IndexScripts.IndexParams
                    ( IndexParams(..)
                    ) where

import Prelude      (Show, Ord(..), Eq(..))
import GHC.Generics (Generic)

import PlutusTx     (makeLift, BuiltinData)

data IndexParams = IndexParams
  { startEndCs       :: !BuiltinData -- ^ init index policy cs as builtin data (linked list boarders)
  , stateAddr        :: !BuiltinData -- ^ global state address where 100 token will be locked
  } deriving stock (Eq, Ord, Show, Generic)

PlutusTx.makeLift ''IndexParams