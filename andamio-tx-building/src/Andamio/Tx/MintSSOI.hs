module Andamio.Tx.MintSSOI
  ( mintSSOI
  ) where

import Prelude                                      (String, Maybe(..), ($), (<>), return)

import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusTx.Builtins                            (BuiltinByteString)
import PlutusTx.Builtins.HasOpaque                  (stringToBuiltinByteString)

import Andamio.GlobalState.OnChain.GlobalStateDatum (GlobalStateDatum(..))
import Andamio.Utility.Tx                           (outDatToDat, unsafeGYOtDatumToData, 
                                                    bbs2ToGyTn, indexTokenName', 
                                                    mustHaveIndexOutput)

-- ^ Build skeleton to mint a global state token and index a new alias. 
-- ^ Has to satisfy "andamio-onchain/src/Andamio/Index/OnChain/IndexScripts/MintingScript.hs"
-- ^ and "andamio-onchain/src/Andamio/Index/OnChain/IndexScripts/SpendingScript.hs"

mintSSOI :: (HasCallStack, GYTxQueryMonad m) 
             => GYTxOutRef         -- ^ index ref tx ref
             -> GYScript 'PlutusV3 -- ^ index ref scr
             -> GYUTxO             -- ^ index utxo
             -> String             -- ^ alias
             -> GYAddress          -- ^ global state validator address
             -> m (GYTxSkeleton 'PlutusV3)
mintSSOI indexRefTxRef indexRefScr indexUtxo alias gsAddr = do
  let tn100 = bbs2ToGyTn "100" aliasBbs
      indexMintingScr = mintingPolicyFromSerialisedScript @'PlutusV3 $ scriptToSerialisedScript indexRefScr
      accessPIds  = mintingPolicyId indexMintingScr
      accessRefScr = GYMintReference indexRefTxRef indexRefScr
      (_, indexValue) = valueSplitAda $ utxoValue indexUtxo
      (this', next') = unsafeGYOtDatumToData @(BuiltinByteString, BuiltinByteString) $ utxoOutDatum indexUtxo
  return $ mustHaveInput (GYTxIn (utxoRef indexUtxo) (GYTxInWitnessScript (GYInReference indexRefTxRef indexRefScr) (outDatToDat $ utxoOutDatum indexUtxo) (redeemerFromPlutusData ())))
        <> mustHaveIndexOutput (utxoAddress indexUtxo) indexValue this' aliasBbs
        <> mustHaveIndexOutput (utxoAddress indexUtxo) (valueSingleton (GYToken accessPIds indexTokenName') 1) aliasBbs next'
        <> mustMint accessRefScr (redeemerFromPlutusData aliasBbs) tn100 1
        <> mustMint accessRefScr (redeemerFromPlutusData aliasBbs) (bbs2ToGyTn "222" aliasBbs) 1
        <> mustMint accessRefScr (redeemerFromPlutusData aliasBbs) indexTokenName' 1
        <> mustHaveOutput GYTxOut 
                            { gyTxOutAddress = gsAddr
                            , gyTxOutValue = valueSingleton (GYToken accessPIds tn100) 1
                            , gyTxOutDatum = Just (datumFromPlutusData $ GlobalStateDatum (mintingPolicyIdToCurrencySymbol accessPIds) aliasBbs [], GYTxOutUseInlineDatum)
                            , gyTxOutRefS = Nothing
                            }
      where
            aliasBbs :: BuiltinByteString 
            aliasBbs = stringToBuiltinByteString alias