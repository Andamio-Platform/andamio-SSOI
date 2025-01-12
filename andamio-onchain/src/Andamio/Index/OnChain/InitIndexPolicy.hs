module Andamio.Index.OnChain.InitIndexPolicy 
                    ( initIndexPolicyCompiledCode
                    , indexTokenNameBd
                    ) where

import qualified Prelude                             as Pr  (Either(..), error)

import           PlutusCore.Version                         (plcVersion110)
import           PlutusTx                                   (compile, CompiledCode, liftCode, applyCode)
import           PlutusTx.Prelude                           (BuiltinUnit, BuiltinData, Bool(..), error, 
                                                            otherwise, (==), (&&), ($))
import           PlutusTx.Builtins.Internal                 (BuiltinList, unsafeDataAsMap, fst, snd, 
                                                            BuiltinPair, unitval, head, tail, mkI, mkB)
import           PlutusTx.Builtins                     as B (null)

import           Andamio.Utility.OnChain.LazyContextV3      (lazyTxInfoMintBd, lazyTxInfoInputsBd, 
                                                            lazyOwnCurrencySymbolBd)
import           Andamio.Utility.OnChain.TxInInfo           (lazyTxInInfoConsumedBd)
import           Andamio.Utility.OnChain.Value              (lengthListTupleBd)

-- ^ index token name as builtin data
{-# INLINEABLE indexTokenNameBd #-}
indexTokenNameBd :: BuiltinData 
indexTokenNameBd = mkB " "

-- ^ Initialization minting script for index defining first and last element of the linked list.

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinList BuiltinData -> Bool
mkPolicy txrefBd txInfoMintBd ownSymbolBd txInfoInputsBd = 
  
        -- ^ tx out ref is consumed for uniqueness
        lazyTxInInfoConsumedBd txInfoInputsBd txrefBd
        -- ^ validate minting value
    &&  checkMinting (unsafeDataAsMap txInfoMintBd)

  where

    -- ^ exactly 2 own symbol tokens with defined token name minted
    checkMinting :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Bool
    checkMinting bdTupleList
      | B.null bdTupleList = error ()
      | fst (head bdTupleList) == ownSymbolBd = go (unsafeDataAsMap $ snd $ head bdTupleList)
      | otherwise = checkMinting (tail bdTupleList)
      where
        go :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Bool
        go bdTupleList' = lengthListTupleBd bdTupleList' 0 == 1 &&
                          fst (head bdTupleList') == indexTokenNameBd &&
                          snd (head bdTupleList') == mkI 2

{-# INLINEABLE untypedPolicy #-}
untypedPolicy :: BuiltinData -> BuiltinData -> BuiltinUnit
untypedPolicy txref ctx'
  | mkPolicy txref (lazyTxInfoMintBd ctx') (lazyOwnCurrencySymbolBd ctx') (lazyTxInfoInputsBd ctx') = unitval
  | otherwise = error ()

initIndexPolicyCompiledCode :: BuiltinData -> CompiledCode (BuiltinData -> BuiltinUnit)
initIndexPolicyCompiledCode txRef = case compiled of
    Pr.Right c -> c
    Pr.Left _ -> Pr.error "initIndexPolicyCompiledCode"
    where
        compiled = $$(compile [||untypedPolicy||]) `applyCode` liftCode plcVersion110 txRef

