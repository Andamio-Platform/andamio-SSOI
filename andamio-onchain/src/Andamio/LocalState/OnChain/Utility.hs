module Andamio.LocalState.OnChain.Utility
    ( checkGlobalRedPresent
    ) where

import PlutusTx.Prelude                        (BuiltinData, Bool(..), (==), error)
import PlutusTx.Builtins.Internal        as BI (BuiltinList(..), BuiltinPair, unitval,
                                               mkConstr, mkCons, mkNilData)
import PlutusLedgerApi.V3                      (Redeemer(..))

import Andamio.Utility.OnChain.Redeemers       (filterRedeemersBdByScriptPurposeBd)

-- ^ True if script purpose and redeemer are present.

{-# INLINEABLE checkGlobalRedPresent #-}
checkGlobalRedPresent :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> BuiltinData -> Bool
checkGlobalRedPresent redeemersBd txOutRef redBd = case filterRedeemersBdByScriptPurposeBd redeemersBd tokenTxRefBd of
    [r'] -> getRedeemer r' == redBd
    _ -> error ()
  where
    tokenTxRefBd :: BuiltinData
    tokenTxRefBd = BI.mkConstr 1 (BI.mkCons txOutRef (BI.mkNilData unitval))