module Andamio.LocalState.OnChain.TokenScripts
                      ( localStateTokenCompiledCode
                      , TokenParams(..)
                      ) where

import qualified Prelude                                         as Pr (error, Either(..))
 
import           PlutusCore.Version                                    (plcVersion110)
import           PlutusTx                                              (compile, liftCode, applyCode, CompiledCode) 
import           PlutusTx.Prelude                                      (BuiltinData, BuiltinUnit, Bool(..), 
                                                                       otherwise, error, ($), (==))
import           PlutusTx.Builtins.Internal                      as BI (head, BuiltinList(..), unsafeDataAsConstr,
                                                                       snd, fst, BuiltinPair, BuiltinInteger, unitval)

import           Andamio.LocalState.OnChain.Token.TokenParams          (TokenParams(..))
import           Andamio.LocalState.OnChain.Token.MintingScript        (validateMinting)
import           Andamio.LocalState.OnChain.Token.SpendingScript       (validateSpending)

import           Andamio.Utility.OnChain.LazyContextV3                 (lazyTxInfoOutputsBd, lazyTxInfoInputsBd,
                                                                       lazyTxInfoMintBd, lazyRedeemerTyped,
                                                                       lazyScriptInfoBd, lazyTxInfoRedeemersBd,
                                                                       lazyTxOutRefBd, lazyInlineDatumTyped)

{-# INLINEABLE untypedValidator #-}
untypedValidator :: TokenParams -> BuiltinData -> BuiltinUnit
untypedValidator sp ctx'
  | spendingOrMinting (BI.unsafeDataAsConstr $ lazyScriptInfoBd ctx') = unitval
  | otherwise = error()
  where
    -- ^ either minting or spending script
    spendingOrMinting :: BuiltinPair BuiltinInteger (BuiltinList BuiltinData) -> Bool
    spendingOrMinting scrInfoBd
      | BI.fst scrInfoBd == 0 = validateMinting sp (lazyRedeemerTyped ctx') (BI.head $ BI.snd scrInfoBd) (lazyTxInfoMintBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx') (lazyTxInfoRedeemersBd ctx')
      | BI.fst scrInfoBd == 1 = validateSpending sp (lazyRedeemerTyped ctx') (lazyInlineDatumTyped ctx') (lazyTxOutRefBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx')
      | otherwise = error ()

localStateTokenCompiledCode :: TokenParams -> CompiledCode (BuiltinData -> BuiltinUnit)
localStateTokenCompiledCode sp = case compiled of 
    Pr.Right c -> c
    Pr.Left _ -> Pr.error "localStateTokenCompiledCode"
    where
        compiled = $$(PlutusTx.compile [||untypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion110 sp