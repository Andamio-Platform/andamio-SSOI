module Andamio.GlobalState.OnChain.GlobalStateValidator 
                    ( globalStateValidatorCompiledCode
                    ) where

import PlutusLedgerApi.V3                                 (CurrencySymbol(..), BuiltinData)
import PlutusTx                                           (CompiledCode, compile, toBuiltinData)    
import PlutusTx.Prelude                                   (Eq(..), (&&), BuiltinUnit, Bool(..), error, 
                                                          otherwise, ($), (<)) 
import PlutusTx.Builtins.Internal                   as BI (BuiltinList, mkB, unsafeDataAsMap, unitval)

import Andamio.GlobalState.OnChain.GlobalStateDatum       (GlobalStateDatum(..))
import Andamio.Utility.OnChain.LazyContextV3              (lazyInlineDatumTyped, lazyTxInfoOutputsBd,
                                                          lazyTxOutRefBd, lazyTxInfoInputsBd,
                                                          lazyTxInfoMintBd, lazyRedeemerTyped)
import Andamio.Utility.OnChain.TxInInfo                   (singleTokenInOutAddrSame,
                                                          findLazyTxInInfoByTxOutRefToTxOutBd)
import Andamio.Utility.OnChain.Value                      (csElemList, valueOfBd, singleTokenInValueBd,
                                                          create222Tn, create100Tn, lengthBuiltinValueIf1Tn)
import Andamio.Utility.OnChain.Datum                      (mkInlineDatumBuiltin, nothingBd)
import Andamio.Utility.OnChain.TxOut                      (lazyTxOutValueMapBd, lazyTxOutAddrBd,
                                                          lazyTxOutReferenceScriptBd,
                                                          findLazyTxOutBySingleToken,
                                                          lazyTxOutDatumBd, lazyTxOutAddrBd)

-- ^ Global state validator handles how local state currency symbols are added. Utilises CIP-68 for ownership of the global state.

{-# INLINEABLE validateSpending #-}
validateSpending :: CurrencySymbol -> GlobalStateDatum -> BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> BuiltinData -> Bool
validateSpending lsCs dat ownInputTxOutRefBd txInfoInputsBd txInfoOutputsBd txInfoMintBd =

              let ownInput = findLazyTxInInfoByTxOutRefToTxOutBd txInfoInputsBd ownInputTxOutRefBd -- ^ own input by tx out ref
                  tn100 = create100Tn (userName dat)                                               -- ^ 100 + user name as builtin data
                  globalCsBd = BI.mkB $ unCurrencySymbol $ globalStateCs dat                       -- ^ global state currency symbol as builtin data
                  txOut100 = findLazyTxOutBySingleToken txInfoOutputsBd globalCsBd tn100           -- ^ global state 100 output tx out
              in
                  -- ^ at least one local state token minted
                  0 < valueOfBd (BI.unsafeDataAsMap txInfoMintBd) (BI.mkB $ unCurrencySymbol lsCs) (BI.mkB $ userName dat)
                  -- ^ local state does not exist in global state
               && csElemList (localStateCss dat) lsCs == False
                  -- ^ own input has global state currency symbol and 100 + user name as token name
               && singleTokenInValueBd (lazyTxOutValueMapBd ownInput) globalCsBd tn100
                  -- ^ own input address is also output address
               && lazyTxOutAddrBd txOut100 == lazyTxOutAddrBd ownInput
                  -- ^ own output has no reference script attached
               && lazyTxOutReferenceScriptBd txOut100 == nothingBd
                  -- ^ own output has exactly 2 currencies attached (ada + global state token)
               && lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd txOut100) 0 == 2
                  -- ^ local state currency symbol added to output global state datum
               && lazyTxOutDatumBd txOut100 == mkInlineDatumBuiltin (toBuiltinData dat{localStateCss=lsCs:localStateCss dat})
                  -- ^ in and out address of global state 222 token is same
               && singleTokenInOutAddrSame txInfoInputsBd txInfoOutputsBd globalCsBd (create222Tn $ userName dat)

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinUnit
untypedValidator ctx'
  | validateSpending (lazyRedeemerTyped ctx') (lazyInlineDatumTyped ctx') (lazyTxOutRefBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx') (lazyTxInfoMintBd ctx') = unitval
  | otherwise = error ()

globalStateValidatorCompiledCode :: CompiledCode (BuiltinData -> BuiltinUnit)
globalStateValidatorCompiledCode = $$(compile [||untypedValidator||])