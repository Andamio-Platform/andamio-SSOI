module Andamio.ProveCreds.OnChain.ProveCredsDatum
                    ( ProveCredsDatum(..)
                    ) where

import           GHC.Generics             (Generic)

import           PlutusTx                 (makeLift, makeIsDataIndexed)
import qualified Prelude            as Pr (Eq, Ord, Show)
import           PlutusTx.Prelude         (Eq(..), (==), (&&), Integer)
import           PlutusLedgerApi.V3       (CurrencySymbol(..), BuiltinByteString)

data ProveCredsDatum = ProveCredsDatum 
  { getBbsCreds :: [(CurrencySymbol, [BuiltinByteString])] -- ^ currency symbol with bbs list attached present
  , getIntCreds :: [(CurrencySymbol, Integer)] -- ^ currency symbol with an integer greater or equal present
  } deriving stock (Pr.Eq, Pr.Ord, Pr.Show, Generic)

instance PlutusTx.Prelude.Eq ProveCredsDatum where
  {-# INLINEABLE (==) #-}
  ProveCredsDatum bC iC == ProveCredsDatum bC' iC' =
    (bC == bC') && (iC == iC')

PlutusTx.makeIsDataIndexed ''ProveCredsDatum [('ProveCredsDatum, 0)]
PlutusTx.makeLift ''ProveCredsDatum