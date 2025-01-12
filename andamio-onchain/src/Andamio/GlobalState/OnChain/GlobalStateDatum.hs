module Andamio.GlobalState.OnChain.GlobalStateDatum
                    ( GlobalStateDatum(..)
                    ) where

import qualified Prelude            as Pr (Eq, Ord, Show)
import           GHC.Generics             (Generic)

import           PlutusTx                 (makeLift, makeIsDataIndexed)
import           PlutusTx.Prelude         (Eq(..), (==), (&&))
import           PlutusLedgerApi.V3       (CurrencySymbol(..), BuiltinByteString)

data GlobalStateDatum = GlobalStateDatum 
  { globalStateCs :: CurrencySymbol    -- ^ currency symbol where global state is attached
  , userName      :: BuiltinByteString -- ^ user name without prefix
  , localStateCss :: [CurrencySymbol]  -- ^ currenc symbols pointing to local states
  } deriving stock (Pr.Eq, Pr.Ord, Pr.Show, Generic)

instance PlutusTx.Prelude.Eq GlobalStateDatum where
  {-# INLINEABLE (==) #-}
  GlobalStateDatum gsCs uN lsI == GlobalStateDatum gsCs' uN' lsI' =
    (gsCs == gsCs') && (uN == uN') && (lsI == lsI') 

PlutusTx.makeIsDataIndexed ''GlobalStateDatum [('GlobalStateDatum, 0)]
PlutusTx.makeLift ''GlobalStateDatum