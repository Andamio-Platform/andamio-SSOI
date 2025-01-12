
module Andamio.Utility.OnChain.TxInInfo 
    ( singleTokenInOutAddrSame
    , findLazyTxInInfoByTxOutRefToTxOutBd
    , lazyTxInInfoTxOutInlineDatBd
    , lazyTxInInfoValueMapBd
    , findLazyTxInInfoByTxOutRefToAddrPkhBd
    , lazyTxInInfoConsumedBd
    , findLazyTxInInfoByToken
    , lazyTxInInfoTxOutRefBd
    , csInTxInInfosBd
    ) where

import PlutusTx.Prelude                          (Bool(..), (==), ($), otherwise, error, (.))
import PlutusTx.Builtins.Internal                (BuiltinData, BuiltinList, BuiltinPair,
                                                 tail, head, unsafeDataAsMap)
import PlutusTx.Builtins                         (null)
import Andamio.Utility.OnChain.Value             (csElem, singleTokenInValueBd)
import Andamio.Utility.OnChain.LazyContextV3     (constrArgs)
import Andamio.Utility.OnChain.Address           (getAddressBdScriptHashBd)
import Andamio.Utility.OnChain.TxOut             (findLazyTxOutBySingleTokenToAddrBd)

-- ^ module to lazy work with TxInInfo

-- ^ get TxOutRef as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> TxOutRef
{-# INLINEABLE lazyTxInInfoTxOutRefBd #-}
lazyTxInInfoTxOutRefBd :: BuiltinData -> BuiltinData
lazyTxInInfoTxOutRefBd = head . constrArgs

-- ^ get TxOut as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> TxOut
{-# INLINEABLE lazyTxInInfoTxOutBd #-}
lazyTxInInfoTxOutBd :: BuiltinData -> BuiltinData
lazyTxInInfoTxOutBd = head . tail . constrArgs

-- ^ get TxOutAddr as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> Address
{-# INLINEABLE lazyTxInInfoTxOutAddrBd #-}
lazyTxInInfoTxOutAddrBd :: BuiltinData -> BuiltinData
lazyTxInInfoTxOutAddrBd = head . constrArgs . lazyTxInInfoTxOutBd

-- ^ get Value as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> Value
{-# INLINEABLE lazyTxInInfoValueBd #-}
lazyTxInInfoValueBd :: BuiltinData -> BuiltinData
lazyTxInInfoValueBd = head . tail . constrArgs . lazyTxInInfoTxOutBd

-- ^ get Value as BuiltinData List Pair from TxInInfo as BuiltinData
-- ^ TxInInfo -> Value
{-# INLINEABLE lazyTxInInfoValueMapBd #-}
lazyTxInInfoValueMapBd :: BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData)
lazyTxInInfoValueMapBd = unsafeDataAsMap . lazyTxInInfoValueBd

-- ^ get OutputDatum as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> OutputDatum
{-# INLINEABLE lazyTxInInfoTxOutOutDatBd #-}
lazyTxInInfoTxOutOutDatBd :: BuiltinData -> BuiltinData
lazyTxInInfoTxOutOutDatBd = head . tail . tail . constrArgs . lazyTxInInfoTxOutBd

-- ^ get OutputDatum (if inline datum) as BuiltinData from TxInInfo as BuiltinData
-- ^ TxInInfo -> Datum
{-# INLINEABLE lazyTxInInfoTxOutInlineDatBd #-}
lazyTxInInfoTxOutInlineDatBd :: BuiltinData -> BuiltinData
lazyTxInInfoTxOutInlineDatBd = head . constrArgs . lazyTxInInfoTxOutOutDatBd

-- ^ validate if a BuiltinData CurrencySymbol is present in Value of BuiltinData TxInInfo
-- ^ [TxInInfo] -> CurrencySymbol -> Bool
{-# INLINEABLE csInTxInInfosBd #-}
csInTxInInfosBd :: BuiltinList BuiltinData -> BuiltinData -> Bool
csInTxInInfosBd bdList cs 
  | null bdList = False
  | csElem (lazyTxInInfoValueMapBd $ head bdList) cs = True
  | otherwise = csInTxInInfosBd (tail bdList) cs

-- ^ find TxInInfo by TxOutRef
-- ^ [TxInInfo] -> TxOutRef -> TxInInfo
{-# INLINEABLE findLazyTxInInfoByTxOutRef #-}
findLazyTxInInfoByTxOutRef :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData
findLazyTxInInfoByTxOutRef bdTxIns txRefBd
  | null bdTxIns = error ()
  | lazyTxInInfoTxOutRefBd (head bdTxIns) == txRefBd = head bdTxIns
  | otherwise = findLazyTxInInfoByTxOutRef (tail bdTxIns) txRefBd

-- ^ find TxOut in  TxInInfo list by TxOutRef
-- ^ [TxInInfo] -> TxOutRef -> TxOut
{-# INLINEABLE findLazyTxInInfoByTxOutRefToTxOutBd #-}
findLazyTxInInfoByTxOutRefToTxOutBd :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData
findLazyTxInInfoByTxOutRefToTxOutBd bdTxIns txRefBd = lazyTxInInfoTxOutBd $ findLazyTxInInfoByTxOutRef bdTxIns txRefBd

--- ^ find PubKeyHash/ScriptHash in TxInInfo list by TxOutRef
-- ^ [TxInInfo] -> TxOutRef -> PubKeyHash/ScriptHash
{-# INLINEABLE findLazyTxInInfoByTxOutRefToAddrPkhBd #-}
findLazyTxInInfoByTxOutRefToAddrPkhBd :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData
findLazyTxInInfoByTxOutRefToAddrPkhBd bdTxIns txRefBd = getAddressBdScriptHashBd $ lazyTxInInfoTxOutAddrBd $ findLazyTxInInfoByTxOutRef bdTxIns txRefBd

-- ^ validate if BuiltinData TxOutRef is consumed in TxInInfo list
-- ^ [TxInInfo] -> TxOutRef -> Bool
{-# INLINEABLE lazyTxInInfoConsumedBd #-}
lazyTxInInfoConsumedBd :: BuiltinList BuiltinData -> BuiltinData -> Bool
lazyTxInInfoConsumedBd txIns txref 
  | null txIns = error ()
  | lazyTxInInfoTxOutRefBd (head txIns) == txref = True
  | otherwise = lazyTxInInfoConsumedBd (tail txIns) txref

-- ^ find TxInInfo by CurrencySymbol and TokenName in TxInInfo list
-- ^ [TxInInfo] -> CurrencySymbol -> TokenName -> TxInInfo
{-# INLINEABLE findLazyTxInInfoByToken #-}
findLazyTxInInfoByToken :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
findLazyTxInInfoByToken bdTxIns cs tn
  | null bdTxIns = error ()
  | singleTokenInValueBd (lazyTxInInfoValueMapBd $ head bdTxIns) cs tn = head bdTxIns
  | otherwise = findLazyTxInInfoByToken (tail bdTxIns) cs tn

-- ^ find Address by CurrencySymbol and TokenName in TxInInfo list
-- ^ [TxInInfo] -> CurrencySymbol -> TokenName -> Address
{-# INLINEABLE findLazyTxInInfoBySingleTokenToAddrBd #-}
findLazyTxInInfoBySingleTokenToAddrBd :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
findLazyTxInInfoBySingleTokenToAddrBd bdTxIns cs tn = lazyTxInInfoTxOutAddrBd $ findLazyTxInInfoByToken bdTxIns cs tn

-- ^ cehck if token has same input and output address
-- ^ [TxInInfo] -> [TxOut] -> CurrencySymbol -> TokenName -> Bool
{-# INLINEABLE singleTokenInOutAddrSame #-}
singleTokenInOutAddrSame :: BuiltinList BuiltinData -> BuiltinList BuiltinData -> BuiltinData -> BuiltinData -> Bool
singleTokenInOutAddrSame txIns txOuts cs tn = findLazyTxInInfoBySingleTokenToAddrBd txIns cs tn == findLazyTxOutBySingleTokenToAddrBd txOuts cs tn