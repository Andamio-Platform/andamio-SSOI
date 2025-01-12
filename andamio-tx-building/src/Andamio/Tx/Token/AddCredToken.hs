module Andamio.Tx.Token.AddCredToken
  ( addCredToken
  ) where

import Prelude                     (String, ($), (<>), return, Maybe(..), 
                                   Integer, (+))
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

import Andamio.Utility.Tx          (outDatToDat, unsafeGYOtDatumToData)

-- ^ Build skeleton to add credential to a local state. 
-- ^ Has to satisfy "andamio-onchain/src/Andamio/LocalState/OnChain/Token/SpendingScript.hs".
-- ^ Called by a credential issuer identified by a global state alias and increases the credential value by 1.

addCredToken :: (HasCallStack, GYTxQueryMonad m)
                 => GYTxOutRef           -- ^ signer own address
                 -> GYUTxO               -- ^ local state utxo
                 -> GYInScript 'PlutusV3 -- ^ local state reference script
                 -> String               -- ^ to cred alias
                 -> m (GYTxSkeleton 'PlutusV3)
addCredToken tokenTxRef localStateUtxo localStateRefScr aliasToCred = do
      let curLocalStateDat = unsafeGYOtDatumToData @Integer (utxoOutDatum localStateUtxo)
      return $ mustHaveInput GYTxIn{gyTxInTxOutRef=tokenTxRef, gyTxInWitness=GYTxInWitnessKey}
            <> mustHaveInput GYTxIn{gyTxInTxOutRef=utxoRef localStateUtxo, 
                  gyTxInWitness=(GYTxInWitnessScript localStateRefScr (outDatToDat $ utxoOutDatum localStateUtxo) (redeemerFromPlutusData $ stringToBuiltinByteString aliasToCred))}
            <> mustHaveOutput GYTxOut
                  { gyTxOutAddress = utxoAddress localStateUtxo
                  , gyTxOutValue = utxoValue localStateUtxo
                  , gyTxOutDatum = Just (datumFromPlutusData (curLocalStateDat + 1), GYTxOutUseInlineDatum)
                  , gyTxOutRefS = Nothing}