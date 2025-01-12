module Andamio.LocalState.OnChain.SignerScripts
                      ( localStateSignerCompiledCode
                      , SignerParams(..)
                      ) where

import qualified Prelude                                          as Pr (error, Either(..))
 
import           PlutusCore.Version                                     (plcVersion110)
import           PlutusTx                                               (compile, liftCode, applyCode, CompiledCode) 
import           PlutusTx.Prelude                                       (BuiltinData, BuiltinUnit, Bool(..), 
                                                                        otherwise, error, ($), (==))
import           PlutusTx.Builtins.Internal                       as BI (head, BuiltinList(..), unsafeDataAsConstr,
                                                                        snd, fst, BuiltinPair, BuiltinInteger, 
                                                                        unsafeDataAsList, unitval)

import           Andamio.LocalState.OnChain.Signer.SignerParams         (SignerParams(..))
import           Andamio.LocalState.OnChain.Signer.MintingScript        (validateMinting)
import           Andamio.LocalState.OnChain.Signer.SpendingScript       (validateSpending)

import           Andamio.Utility.OnChain.LazyContextV3                  (lazyTxInfoOutputsBd, lazyTxInfoInputsBd,
                                                                        lazyTxInfoMintBd, lazyRedeemerTyped,
                                                                        lazyScriptInfoBd, lazyTxInfoRedeemersBd,
                                                                        lazyTxInfoSignatoriesBd, lazyTxOutRefBd,
                                                                        lazyInlineDatumTyped)

{-# INLINEABLE untypedValidator #-}
untypedValidator :: SignerParams -> BuiltinData -> BuiltinUnit
untypedValidator sp ctx'
  | spendingOrMinting (BI.unsafeDataAsConstr $ lazyScriptInfoBd ctx') = unitval
  | otherwise = error()
  where
    -- ^ either minting or spending script
    spendingOrMinting :: BuiltinPair BuiltinInteger (BuiltinList BuiltinData) -> Bool
    spendingOrMinting scrInfoBd
      | BI.fst scrInfoBd == 0 = validateMinting sp (lazyRedeemerTyped ctx') (BI.head $ BI.snd scrInfoBd) (lazyTxInfoMintBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx') (lazyTxInfoRedeemersBd ctx')
      | BI.fst scrInfoBd == 1 = validateSpending sp (lazyRedeemerTyped ctx') (lazyInlineDatumTyped ctx') (lazyTxOutRefBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx') (BI.unsafeDataAsList $ lazyTxInfoSignatoriesBd ctx')
      | otherwise = error ()

localStateSignerCompiledCode :: SignerParams -> CompiledCode (BuiltinData -> BuiltinUnit)
localStateSignerCompiledCode sp = case compiled of 
    Pr.Right c -> c
    Pr.Left _ -> Pr.error "localStateSignerCompiledCode"
    where
        compiled = $$(PlutusTx.compile [||untypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion110 sp