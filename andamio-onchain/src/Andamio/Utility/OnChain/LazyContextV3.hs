module Andamio.Utility.OnChain.LazyContextV3
  ( lazyInlineDatumTyped
  , lazyTxInfoOutputsBd
  , lazyTxOutRefBd
  , lazyTxInfoInputsBd
  , lazyTxInfoMintBd
  , lazyRedeemerTyped
  , lazyScriptInfoBd
  , lazyTxInfoRedeemersBd
  , lazyOwnCurrencySymbolBd
  , lazyTxInfoSignatoriesBd
  , lazyTxInfoReferenceInputsBd
  , constrArgs
  )
where

import          PlutusLedgerApi.V3         (Redeemer(..), Datum(..))
import          PlutusTx                   (BuiltinData, UnsafeFromData, unsafeFromBuiltinData)
import          PlutusTx.Builtins.Internal (head, tail, snd, BuiltinList, unsafeDataAsConstr,
                                           unsafeDataAsList, BuiltinPair, unsafeDataAsMap)
import          PlutusTx.Prelude           ((.))

-- ^ module to lazy work with V3 ScriptContext

-- ^ get unsafe data list from constructor
{-# INLINEABLE constrArgs #-}
constrArgs :: BuiltinData -> BuiltinList BuiltinData
constrArgs = snd . unsafeDataAsConstr

-- ^ data in ScriptContext
-- ^ ScriptContext
{-# INLINEABLE getContext #-}
getContext :: BuiltinData -> BuiltinList BuiltinData
getContext = constrArgs

-- ^ scriptContextRedeemer from ScriptContext typed
{-# INLINEABLE lazyRedeemerTyped #-}
lazyRedeemerTyped :: forall red. (UnsafeFromData red) => BuiltinData -> red
lazyRedeemerTyped = unsafeFromBuiltinData @red . getRedeemer . unsafeFromBuiltinData . head . tail . getContext

-- ^ scriptContextScriptInfo from ScriptContext as BuiltinData
-- ^ ScriptContext -> ScriptInfo
{-# INLINEABLE lazyScriptInfoBd #-}
lazyScriptInfoBd :: BuiltinData -> BuiltinData
lazyScriptInfoBd = head . tail . tail . getContext

-- ^ get typed datum from ScriptContext if SpendingScript and InlineDatum
{-# INLINEABLE lazyInlineDatumTyped #-}
lazyInlineDatumTyped :: forall dat. (UnsafeFromData dat) => BuiltinData -> dat
lazyInlineDatumTyped = unsafeFromBuiltinData @dat . getDatum . unsafeFromBuiltinData . head . constrArgs  . head . tail . constrArgs . lazyScriptInfoBd

-- ^ get TxOutRef as BuiltinData from ScriptContext if SpendingScript
-- ^ ScriptContext -> TxOutRef
{-# INLINEABLE lazyTxOutRefBd #-}
lazyTxOutRefBd :: BuiltinData -> BuiltinData
lazyTxOutRefBd = head . constrArgs . lazyScriptInfoBd

-- ^ get CurrencySymbol as BuiltinData from ScriptContext if MintingScript
-- ^ ScriptContext -> CurrencySymbol
{-# INLINEABLE lazyOwnCurrencySymbolBd #-}
lazyOwnCurrencySymbolBd :: BuiltinData -> BuiltinData
lazyOwnCurrencySymbolBd = head . constrArgs . lazyScriptInfoBd

-- ^ scriptContextTxInfo from ScriptContext as BuiltinData
-- ^ ScriptContext -> TxInfo
{-# INLINEABLE lazyTxInfo #-}
lazyTxInfo :: BuiltinData -> BuiltinList BuiltinData
lazyTxInfo = constrArgs . head . getContext

-- ^ txInfoInputs fromScriptContext as BuiltinData
-- ^ ScriptContext -> [TxInInfo]
{-# INLINEABLE lazyTxInfoInputsBd #-}
lazyTxInfoInputsBd :: BuiltinData -> BuiltinList BuiltinData
lazyTxInfoInputsBd = unsafeDataAsList . head . lazyTxInfo

-- ^ txInfoReferenceInputs fromScriptContext as BuiltinData
-- ^ ScriptContext -> [TxInInfo]
{-# INLINEABLE lazyTxInfoReferenceInputsBd #-}
lazyTxInfoReferenceInputsBd :: BuiltinData -> BuiltinList BuiltinData
lazyTxInfoReferenceInputsBd = unsafeDataAsList . head . tail . lazyTxInfo

-- ^ txInfoOutputs fromScriptContext as BuiltinData
-- ^ ScriptContext -> [TxOut]
{-# INLINEABLE lazyTxInfoOutputsBd #-}
lazyTxInfoOutputsBd :: BuiltinData -> BuiltinList BuiltinData
lazyTxInfoOutputsBd = unsafeDataAsList . head . tail . tail . lazyTxInfo

-- ^ txInfoMint fromScriptContext as BuiltinData
-- ^ ScriptContext -> Value
{-# INLINEABLE lazyTxInfoMintBd #-}
lazyTxInfoMintBd :: BuiltinData -> BuiltinData
lazyTxInfoMintBd = head . tail . tail . tail . tail . lazyTxInfo

-- ^ txInfoSignatories fromScriptContext as BuiltinData
-- ^ ScriptContext -> [PubKeyHash]
{-# INLINEABLE lazyTxInfoSignatoriesBd #-}
lazyTxInfoSignatoriesBd :: BuiltinData -> BuiltinData
lazyTxInfoSignatoriesBd = head . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

-- ^ txInfoRedeemers fromScriptContext as BuiltinData
-- ^ ScriptContext -> Map ScriptPurpose Redeemer
{-# INLINEABLE lazyTxInfoRedeemersBd #-}
lazyTxInfoRedeemersBd :: BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData)
lazyTxInfoRedeemersBd = unsafeDataAsMap . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo