module Andamio.Utility.OnChain.Redeemers
    ( filterRedeemersBdByScriptPurposeBd
    ) where

import PlutusTx.Prelude           (otherwise, ($), (==))
import PlutusTx.Builtins.Internal (BuiltinData, BuiltinPair, 
                                  tail, head, snd, fst, BuiltinList)
import PlutusTx.Builtins          (null)
import PlutusLedgerApi.V3         (Redeemer, unsafeFromBuiltinData)

-- ^ filter redeemers by script purpose and return Redeemer list
-- ^ Map ScriptPurpose Redeemer -> ScriptPurpose -> [Redeemer]
{-# INLINEABLE filterRedeemersBdByScriptPurposeBd #-}
filterRedeemersBdByScriptPurposeBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> [Redeemer]
filterRedeemersBdByScriptPurposeBd bdListTuple purposeBd = go bdListTuple []
  where
    go :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> [Redeemer] -> [Redeemer]
    go bdListTuple' !counter
      | null bdListTuple' = counter
      | fst (head bdListTuple') == purposeBd = go (tail bdListTuple') (unsafeFromBuiltinData (snd $ head bdListTuple'):counter)
      | otherwise = go (tail bdListTuple') counter