
module Andamio.Utility.OnChain.TxOut 
    ( lazyTxOutValueMapBd
    , lazyTxOutReferenceScriptBd
    , findLazyTxOutBySingleToken
    , lazyTxOutDatumBd
    , lazyTxOutAddrBd
    , findLazyTxOutBySingleTokenToAddrBd
    ) where

import PlutusTx.Prelude                      (($), otherwise, error, (.))
import PlutusTx.Builtins.Internal            (BuiltinList, BuiltinData, BuiltinPair, 
                                             tail, head, unsafeDataAsMap)
import PlutusTx.Builtins                     (null)
import Andamio.Utility.OnChain.Value         (singleTokenInValueBd)
import Andamio.Utility.OnChain.LazyContextV3 (constrArgs)

-- ^ TxOut -> Address
{-# INLINEABLE lazyTxOutAddrBd #-}
lazyTxOutAddrBd :: BuiltinData -> BuiltinData
lazyTxOutAddrBd = head . constrArgs

-- ^ TxOut -> Value
{-# INLINEABLE lazyTxOutValueBd #-}
lazyTxOutValueBd :: BuiltinData -> BuiltinData
lazyTxOutValueBd = head . tail . constrArgs

-- ^ TxOut -> Map CurrencySymbol Map TokenName Integer
{-# INLINEABLE lazyTxOutValueMapBd #-}
lazyTxOutValueMapBd :: BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData)
lazyTxOutValueMapBd = unsafeDataAsMap . lazyTxOutValueBd

-- ^ TxOut -> OutputDatum
{-# INLINEABLE lazyTxOutDatumBd #-}
lazyTxOutDatumBd :: BuiltinData -> BuiltinData
lazyTxOutDatumBd = head . tail . tail . constrArgs

-- ^ TxOut -> Maybe ReferenceScript
{-# INLINEABLE lazyTxOutReferenceScriptBd #-}
lazyTxOutReferenceScriptBd :: BuiltinData -> BuiltinData
lazyTxOutReferenceScriptBd = head . tail . tail . tail . constrArgs

-- ^ find TxOut by single token
-- ^ [TxOut] -> CurrencySymbol -> TokenName -> TxOut
{-# INLINEABLE findLazyTxOutBySingleToken #-}
findLazyTxOutBySingleToken :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
findLazyTxOutBySingleToken txOutsBd cs tn
  | null txOutsBd = error ()
  | singleTokenInValueBd (lazyTxOutValueMapBd $ head txOutsBd) cs tn = head txOutsBd
  | otherwise = findLazyTxOutBySingleToken (tail txOutsBd) cs tn

-- ^ find TxOut by single token and return Address
-- ^ [TxOut] -> CurrencySymbol -> TokenName -> Address
{-# INLINEABLE findLazyTxOutBySingleTokenToAddrBd #-}
findLazyTxOutBySingleTokenToAddrBd :: BuiltinList BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
findLazyTxOutBySingleTokenToAddrBd txOutsBd cs tn = lazyTxOutAddrBd $ findLazyTxOutBySingleToken txOutsBd cs tn
