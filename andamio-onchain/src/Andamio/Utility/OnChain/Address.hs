module Andamio.Utility.OnChain.Address
    ( addressFromScriptHashNoStakeBd
    , getAddressBdScriptHashBd
    ) where

import PlutusTx.Prelude                            (($), BuiltinData, (.))
import PlutusTx.Builtins.Internal            as BI (mkConstr, mkNilData, mkCons, unitval, head)
import Andamio.Utility.OnChain.LazyContextV3       (constrArgs)


-- ^ creates a Address as BuiltinData from a BuiltinData ScriptHash with no staking part
-- ^ ScriptHash -> Address
{-# INLINEABLE addressFromScriptHashNoStakeBd #-}
addressFromScriptHashNoStakeBd :: BuiltinData -> BuiltinData
addressFromScriptHashNoStakeBd pkh = BI.mkConstr 0 (BI.mkCons (scriptCredFromScrHashBd pkh) (BI.mkCons (mkConstr 1 (BI.mkNilData unitval)) (BI.mkNilData unitval)))

-- ^ creates ScriptCredential as BuiltinData from BuiltinData ScriptHash
-- ^ ScriptHash -> ScriptCredential
{-# INLINEABLE scriptCredFromScrHashBd #-}
scriptCredFromScrHashBd :: BuiltinData -> BuiltinData
scriptCredFromScrHashBd sh = BI.mkConstr 1 (BI.mkCons sh $ BI.mkNilData unitval)

-- ^ get ScriptHash/PubKeyHash as BuiltinData from BuiltinData Address
-- ^ Address -> (ScriptHash | PubKeyHash)
{-# INLINEABLE getAddressBdScriptHashBd #-}
getAddressBdScriptHashBd :: BuiltinData -> BuiltinData
getAddressBdScriptHashBd = head . constrArgs . head . constrArgs