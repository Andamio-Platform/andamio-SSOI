module Andamio.LocalState.OnChain.Signer.SignerParams
                    ( SignerParams(..)
                    ) where

import Prelude      (Show, Ord(..), Eq(..))
import GHC.Generics (Generic)

import PlutusTx     (makeLift, BuiltinData)

data SignerParams = SignerParams
  { spSignerPkh     :: !BuiltinData -- ^ public key hash allowed to add something to datum
  , spGlobalCs      :: !BuiltinData -- ^ global currency symbol allowed to mint a local state
  } deriving stock (Eq, Ord, Show, Generic)

PlutusTx.makeLift ''SignerParams