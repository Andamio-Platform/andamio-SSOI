module Andamio.Utility.OnChain.Datum
    ( mkInlineDatumBuiltin
    , nothingBd
    ) where

import PlutusTx.Prelude                    (BuiltinData)
import PlutusTx.Builtins.Internal    as BI (mkConstr, mkCons, mkNilData, unitval)

-- ^ creates inline datum as BuiltinData from BuiltinData
-- ^ BuiltinData -> OutputDatum
{-# INLINEABLE mkInlineDatumBuiltin #-}
mkInlineDatumBuiltin :: BuiltinData -> BuiltinData
mkInlineDatumBuiltin bd = mkConstr 2 (BI.mkCons bd (BI.mkNilData unitval))

-- ^ simple Nothing as BuiltinData
-- ^ Nothing
{-# INLINEABLE nothingBd #-}
nothingBd :: BuiltinData
nothingBd = mkConstr 1 (BI.mkNilData unitval)