module Andamio.Tx.ProveCredentials
  ( proveCredentials
  ) where

import Prelude                     (String, ($), (<>), return, map, mconcat)

import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)

import Andamio.Utility.Tx          (outDatToDat)

-- ^ Build skeleton to prove the ownership of credentials.
-- ^ Has to satisfy "andamio-onchain/src/Andamio/ProveCreds/OnChain/ProveCredsValidator.hs"
-- ^ Called by an owner of credentials required by a given reward transaction.

proveCredentials :: (HasCallStack, GYTxQueryMonad m) 
             => GYTxOutRef           -- ^ global 222 token tx ref
             -> [GYTxOutRef]         -- ^ local state credential prove tx refs
             -> GYUTxO               -- ^ prove credential reward utxo
             -> GYInScript 'PlutusV3 -- ^ prove credential reference script
             -> String               -- ^ alias
             -> m (GYTxSkeleton 'PlutusV3)
proveCredentials global222TxRef proveCredsTxRefs rewardUtxo proveCredRefScr alias = do
  return $ mustHaveInput GYTxIn{gyTxInTxOutRef=global222TxRef, gyTxInWitness=GYTxInWitnessKey}
        <> mconcat (map mustHaveRefInput proveCredsTxRefs)
        <> mustHaveInput GYTxIn {gyTxInTxOutRef=utxoRef rewardUtxo, 
                                 gyTxInWitness=GYTxInWitnessScript proveCredRefScr (outDatToDat $ utxoOutDatum rewardUtxo) (redeemerFromPlutusData $ stringToBuiltinByteString alias)}