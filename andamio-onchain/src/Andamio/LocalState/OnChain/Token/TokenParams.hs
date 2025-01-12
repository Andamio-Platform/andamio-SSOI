module Andamio.LocalState.OnChain.Token.TokenParams
                    ( TokenParams(..)
                    ) where

import Prelude          (Show, Ord(..), Eq(..))
import GHC.Generics     (Generic)

import PlutusTx         (makeLift, BuiltinData)
import PlutusTx.Prelude (BuiltinByteString)

data TokenParams = TokenParams
  { tpGlobalAlias   :: !BuiltinByteString -- ^ global currency alias, no prefix
  , tpGlobalCs      :: !BuiltinData       -- ^ global currency symbol allowed to mint a local state
  } deriving stock (Eq, Ord, Show, Generic)

PlutusTx.makeLift ''TokenParams