module Andamio.Tx.Signer.AddCredSigner
  ( addCredSigner
  ) where

import Prelude                     (String, ($), (<>), return, Maybe(..))
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx.Prelude            (BuiltinByteString)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

import Andamio.Utility.Tx          (outDatToDat, unsafeGYOtDatumToData)

-- ^ Build skeleton to add credential to a local state. 
-- ^ Has to satisfy "andamio-onchain/src/Andamio/LocalState/OnChain/Signer/SpendingScript.hs".
-- ^ Called by a credential issuer identified by a public key hash provides a string which is the credential.

addCredSigner :: (HasCallStack, GYTxQueryMonad m)
                 => GYAddress            -- ^ signer own address
                 -> GYUTxO               -- ^ local state utxo
                 -> GYInScript 'PlutusV3 -- ^ local state reference script
                 -> String               -- ^ to cred alias
                 -> String               -- ^ cred to add
                 -> m (GYTxSkeleton 'PlutusV3)
addCredSigner ownAddr localStateUtxo localStateRefScr aliasToCred cred = do
      ownPkh <- addressToPubKeyHash' ownAddr
      let curLocalStateDat = unsafeGYOtDatumToData @[BuiltinByteString] (utxoOutDatum localStateUtxo)
      return $ mustBeSignedBy ownPkh
            <> mustHaveInput GYTxIn{gyTxInTxOutRef=utxoRef localStateUtxo, 
                  gyTxInWitness=(GYTxInWitnessScript localStateRefScr (outDatToDat $ utxoOutDatum localStateUtxo) (redeemerFromPlutusData (strToBbs aliasToCred, strToBbs cred)))}
            <> mustHaveOutput GYTxOut
                  { gyTxOutAddress = utxoAddress localStateUtxo
                  , gyTxOutValue = utxoValue localStateUtxo
                  , gyTxOutDatum = Just (datumFromPlutusData ((strToBbs cred):curLocalStateDat), GYTxOutUseInlineDatum)
                  , gyTxOutRefS = Nothing}
      where
            strToBbs :: String -> BuiltinByteString
            strToBbs = stringToBuiltinByteString