module Andamio.Tx.InitSystem
  ( initIndexGs
  ) where

import           Prelude                (($), Maybe(..), (<>), return, mempty)
import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           Andamio.Utility.Tx     (mustHaveIndexOutput, indexTokenName')

-- ^ Build skeleton to mint the boarders of the index linked list and creates all reference scripts.
-- ^ Has to satisfy "andamio-onchain/src/Andamio/Index/OnChain/InitIndexPolicy.hs"

initIndexGs :: (HasCallStack, GYTxQueryMonad m)
            => GYTxOutRef          -- ^ tx ref to be consumed for init policy
            -> GYScript 'PlutusV3  -- ^ init policy
            -> GYScript 'PlutusV3  -- ^ index script
            -> GYScript 'PlutusV3  -- ^ global state validator
            -> GYScript 'PlutusV3  -- ^ prove credential validator
            -> GYAddress           -- ^ reference wallet address
            -> m (GYTxSkeleton 'PlutusV3)
initIndexGs txRef initPolicy indexVal globalStateScr proveCredValidator refAddr = do
    nid <- networkId
    let indexAddr = addressFromScriptHash nid (scriptHash indexVal)
        initValue = valueSingleton (GYToken (mintingPolicyId initPolicy) indexTokenName') 1
    return $
             mustHaveInput GYTxIn{gyTxInTxOutRef=txRef, gyTxInWitness=GYTxInWitnessKey}
          <> mustMint (GYMintScript initPolicy) (redeemerFromPlutusData ()) indexTokenName' 2
          <> mkRefScrOut indexVal
          <> mkRefScrOut globalStateScr
          <> mkRefScrOut proveCredValidator
          <> mustHaveIndexOutput indexAddr initValue " " "~"
          <> mustHaveIndexOutput indexAddr initValue "~" "~"  
      where
            mkRefScrOut :: GYScript 'PlutusV3 -> GYTxSkeleton 'PlutusV3
            mkRefScrOut scr = mustHaveOutput GYTxOut
                { gyTxOutAddress = refAddr
                , gyTxOutValue = mempty
                , gyTxOutDatum = Nothing
                , gyTxOutRefS = Just $ GYPlutusScript scr
                }