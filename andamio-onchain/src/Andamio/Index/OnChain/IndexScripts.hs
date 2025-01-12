module Andamio.Index.OnChain.IndexScripts
                    ( IndexParams(..)
                    , indexScriptsCompiledCode
                    ) where

import qualified Prelude                    as Pr  (error, Either(..))
 
import           PlutusCore.Version                (plcVersion110)
import           PlutusTx                          (compile, liftCode, applyCode, CompiledCode)  
import           PlutusTx.Prelude           as PPr (BuiltinUnit, BuiltinData, Bool(..), 
                                                   ($), otherwise, error, (==))

import           PlutusTx.Builtins.Internal as BI  (head, BuiltinList(..), unsafeDataAsConstr,
                                                   snd, fst, BuiltinPair, BuiltinInteger, unitval)

import           Andamio.Utility.OnChain.LazyContextV3              (lazyTxInfoOutputsBd, lazyTxInfoInputsBd,
                                                                    lazyTxInfoMintBd, lazyRedeemerTyped,
                                                                    lazyScriptInfoBd, lazyTxInfoRedeemersBd)
import           Andamio.Utility.OnChain.TxInInfo                   (findLazyTxInInfoByTxOutRefToAddrPkhBd)

import           Andamio.Index.OnChain.IndexScripts.IndexParams    (IndexParams(..))
import           Andamio.Index.OnChain.IndexScripts.SpendingScript (validateSpending)
import           Andamio.Index.OnChain.IndexScripts.MintingScript  (validateMinting)

{-# INLINEABLE untypedValidator #-}
untypedValidator :: IndexParams -> BuiltinData -> BuiltinUnit
untypedValidator params ctx'
  | spendingOrMinting (BI.unsafeDataAsConstr $ lazyScriptInfoBd ctx') = unitval
  | otherwise = error()
  where
    -- ^ either minting or spending script
    spendingOrMinting :: BuiltinPair BuiltinInteger (BuiltinList BuiltinData) -> Bool
    spendingOrMinting scrInfoBd
      | BI.fst scrInfoBd == 0 = validateMinting params (lazyRedeemerTyped ctx') (BI.head $ BI.snd scrInfoBd) (lazyTxInfoMintBd ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx')
      | BI.fst scrInfoBd == 1 = validateSpending (findLazyTxInInfoByTxOutRefToAddrPkhBd (lazyTxInfoInputsBd ctx') (BI.head $ BI.snd scrInfoBd)) (lazyTxInfoRedeemersBd ctx') 0
      | otherwise = error ()

indexScriptsCompiledCode :: IndexParams -> CompiledCode (BuiltinData -> BuiltinUnit)
indexScriptsCompiledCode ip = case compiled of 
    Pr.Right c -> c
    Pr.Left _ -> Pr.error "indexScriptsCompiledCode"
    where
        compiled = $$(PlutusTx.compile [||untypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion110 ip 