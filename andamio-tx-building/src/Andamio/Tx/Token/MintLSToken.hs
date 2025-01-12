module Andamio.Tx.Token.MintLSToken
  ( mintLSToken
  ) where

import Prelude                                      (Maybe(..), (<>), snd, ($), return, Integer)
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import Andamio.GlobalState.OnChain.GlobalStateDatum (GlobalStateDatum(..))

import Andamio.Utility.Tx                           (outDatToDat, unsafeGYOtDatumToData, bbsToGyTn)

-- ^ Build skeleton to mint a local state for an issuer identified by a global state alias. 
-- ^ Has to satisfy "andamio-onchain/src/Andamio/LocalState/OnChain/Token/MintingScript.hs"
-- ^ and "andamio-onchain/src/Andamio/GlobalState/OnChain/GlobalStateValidator.hs"
-- ^ Called by an owner of a global state token.

mintLSToken :: (HasCallStack, GYTxQueryMonad m)
               => GYTxOutRef             -- ^ 222 alias tx ref
               -> GYUTxO                 -- ^ 100 alias utxo
               -> GYInScript 'PlutusV3   -- ^ global state ref script
               -> GYMintScript 'PlutusV3 -- ^ local state script
               -> GYAddress              -- ^ local state validator address
               -> m (GYTxSkeleton 'PlutusV3)
mintLSToken alias222txref alias100utxo gsRefScr lsRefScr lsAddr = do
      let curGsDat = outDatToDat $ utxoOutDatum alias100utxo
          gsDat = unsafeGYOtDatumToData @GlobalStateDatum (utxoOutDatum alias100utxo)
          lsPId = mintingPolicyIdFromWitness lsRefScr
      return $ mustHaveInput GYTxIn{gyTxInTxOutRef=alias222txref, gyTxInWitness=GYTxInWitnessKey}
            <> mustMint lsRefScr (redeemerFromPlutusData (userName gsDat)) (bbsToGyTn $ userName gsDat) 1
            <> mustHaveInput GYTxIn{gyTxInTxOutRef=utxoRef alias100utxo, 
                  gyTxInWitness=(GYTxInWitnessScript gsRefScr curGsDat (redeemerFromPlutusData $ mintingPolicyIdToCurrencySymbol lsPId))}
            <> mustHaveOutput GYTxOut 
                  { gyTxOutAddress = utxoAddress alias100utxo
                  , gyTxOutValue = snd $ valueSplitAda (utxoValue alias100utxo)
                  , gyTxOutDatum = Just (datumFromPlutusData gsDat{localStateCss=mintingPolicyIdToCurrencySymbol lsPId:localStateCss gsDat}, GYTxOutUseInlineDatum)
                  , gyTxOutRefS = Nothing}
            <> mustHaveOutput GYTxOut
                  { gyTxOutAddress = lsAddr
                  , gyTxOutValue = valueSingleton (GYToken lsPId (bbsToGyTn $ userName gsDat)) 1
                  , gyTxOutDatum = Just (datumFromPlutusData (0 :: Integer), GYTxOutUseInlineDatum)
                  , gyTxOutRefS = Nothing}