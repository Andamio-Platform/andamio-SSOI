module Main (main) where

import Data.Maybe (fromJust)
import Prelude
import           Test.Tasty                       (testGroup, defaultMain)

import Test.Tasty.HUnit (testCaseSteps)

import GeniusYield.Imports
import GeniusYield.Types
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Test.Privnet.Asserts

import Andamio.Tx
import Andamio.Utility.Tx

debug :: String -> IO ()
-- FIXME: change me to debug setup code.
debug = putStrLn
--debug _ = return ()

main :: IO ()
main = do
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $
      testGroup
        "ssoi-privnet-tests" 
              [ testCaseSteps "full run: init to credential" $ \info -> withSetup info setup $ \ctx -> do

                providerTxRefs <- gyQueryUtxoRefsAtAddress (ctxProviders ctx) (userAddr $ ctxUserF ctx)
                gsAddr <- ctxRunQuery ctx $ scriptAddress globalStateValidator
                newPaymentSKey <- generatePaymentSigningKey

                let initPol = initIndexPolicy (providerTxRefs!!0)
                    indexVal = indexScript (mintingPolicyId initPol) gsAddr
                    proveVal = proveCredsScript (mintingPolicyId indexVal)
                    refAddr = addressFromPaymentKeyHash (ctxNetworkId ctx) $ paymentKeyHash (paymentVerificationKey newPaymentSKey)

                debug $ "init system"
                initTxBody <- ctxRun ctx (ctxUserF ctx) $ do
                  
                  initSkeleton <- initIndexGs (providerTxRefs!!0) initPol indexVal globalStateValidator proveVal (userAddr $ ctxUserF ctx)
                  initTxBody <- buildTxBody initSkeleton
                  signAndSubmitConfirmed_ initTxBody
                  pure initTxBody

                indexRefScrTxRef <- getRefScrTxRef initTxBody indexVal
                gsRefScrTxRef <- getRefScrTxRef initTxBody globalStateValidator
                proveCredRefScrTxRef <- getRefScrTxRef initTxBody proveVal
                indexAddr <- ctxRunQuery ctx $ scriptAddress indexVal
                proveAddr <- ctxRunQuery ctx $ scriptAddress proveVal

                let user2SignerScript = localStateSignerScript (fromJust $ addressToPubKeyHash $ userAddr $ ctxUser2 ctx) (mintingPolicyId indexVal)
                    user2TokenScript = localStateTokenScript "user2" (mintingPolicyId indexVal)
                user2SignerAddr <- ctxRunQuery ctx $ scriptAddress user2SignerScript
                user2TokenAddr <- ctxRunQuery ctx $ scriptAddress user2TokenScript
                indexUtxos <- gyQueryUtxosAtAddress (ctxProviders ctx) indexAddr Nothing

                debug $ "user2 mint ssoi and create credential issuer scripts"
                (signer2RefScrTxRef, token2RefScrTxRef) <- ctxRun ctx (ctxUser2 ctx) $ do

                  ssoiSkeleton <- mintSSOI indexRefScrTxRef indexVal (findIndexUtxoByNewElement (utxosToList indexUtxos) "user2") "user2" gsAddr
                  ssoiTxBody  <- buildTxBody ssoiSkeleton
                  signAndSubmitConfirmed_ ssoiTxBody

                  user2RefScrTxBody <- buildTxBody $ mustHaveOutput (mkRefScrTxOut refAddr user2SignerScript) 
                                                  <> mustHaveOutput (mkRefScrTxOut refAddr user2TokenScript)
                                                  <> mustHaveOutput GYTxOut
                                                                    { gyTxOutAddress = proveAddr
                                                                    , gyTxOutValue = valueSingleton (ctxIron ctx) 100
                                                                    , gyTxOutDatum = Just (datumFromPlutusData $ createProveCredsDatum [(mintingPolicyId user2SignerScript, ["some credential"])] [(mintingPolicyId user2TokenScript, 1)], GYTxOutUseInlineDatum)
                                                                    , gyTxOutRefS = Nothing
                                                                    }
                  signAndSubmitConfirmed_ user2RefScrTxBody

                  signer2RefScrTxRef <- getRefScrTxRef' user2RefScrTxBody user2SignerScript
                  token2RefScrTxRef  <- getRefScrTxRef' user2RefScrTxBody user2TokenScript

                  pure (signer2RefScrTxRef, token2RefScrTxRef)
                
                indexUtxos' <- gyQueryUtxosAtAddress (ctxProviders ctx) indexAddr Nothing

                debug $ "user3 mint ssoi and local states"
                ctxRun ctx (ctxUser3 ctx) $ do

                  ssoiSkeleton <- mintSSOI indexRefScrTxRef indexVal (findIndexUtxoByNewElement (utxosToList indexUtxos') "user3") "user3" gsAddr
                  ssoiTxBody  <- buildTxBody ssoiSkeleton
                  signAndSubmitConfirmed_ ssoiTxBody

                  user222Utxo <- getAssetClassTxRef ssoiTxBody (GYToken (mintingPolicyId indexVal) "222user3")
                  user100Utxo <- getAssetClassTxRef ssoiTxBody (GYToken (mintingPolicyId indexVal) "100user3")

                  lsSignerSkeleton <- mintLSSigner (utxoRef user222Utxo) user100Utxo (GYInReference gsRefScrTxRef globalStateValidator) (GYMintReference signer2RefScrTxRef user2SignerScript) user2SignerAddr
                  lsSignerTxBody  <- buildTxBody lsSignerSkeleton
                  signAndSubmitConfirmed_ lsSignerTxBody

                  user222Utxo' <- getAssetClassTxRef lsSignerTxBody (GYToken (mintingPolicyId indexVal) "222user3")
                  user100Utxo' <- getAssetClassTxRef lsSignerTxBody (GYToken (mintingPolicyId indexVal) "100user3")

                  lsTokenSkeleton <- mintLSToken (utxoRef user222Utxo') user100Utxo' (GYInReference gsRefScrTxRef globalStateValidator) (GYMintReference token2RefScrTxRef user2TokenScript) user2TokenAddr
                  lsTokenTxBody  <- buildTxBody lsTokenSkeleton
                  signAndSubmitConfirmed_ lsTokenTxBody
                
                localSignerState <- gyQueryUtxosAtAddress (ctxProviders ctx) user2SignerAddr (Just $ GYToken (mintingPolicyId user2SignerScript) "user3")
                localTokenState <- gyQueryUtxosAtAddress (ctxProviders ctx) user2TokenAddr (Just $ GYToken (mintingPolicyId user2TokenScript) "user3")
                user2Token222Utxos <- gyQueryUtxosAtAddress (ctxProviders ctx) (userAddr $ ctxUser2 ctx) (Just $ GYToken (mintingPolicyId indexVal) "222user2")

                debug $ "user2 issue credential"
                ctxRun ctx (ctxUser2 ctx) $ do

                  signerAddCredsSkeleton <- addCredSigner (userAddr $ ctxUser2 ctx) (utxosToHeadUtxo localSignerState) (GYInReference signer2RefScrTxRef user2SignerScript) "user3" "some credential"
                  signerAddCredsTxBody  <- buildTxBody signerAddCredsSkeleton
                  signAndSubmitConfirmed_ signerAddCredsTxBody

                  tokenAddCredsSkeleton <- addCredToken (utxosToUtxoRef user2Token222Utxos) (utxosToHeadUtxo localTokenState) (GYInReference token2RefScrTxRef user2TokenScript) "user3"
                  tokenAddCredsTxBody  <- buildTxBody tokenAddCredsSkeleton
                  signAndSubmitConfirmed_ tokenAddCredsTxBody

                global222TxRef <- gyQueryUtxosAtAddress (ctxProviders ctx) (userAddr $ ctxUser3 ctx) (Just $ GYToken (mintingPolicyId indexVal) "222user3")
                localSignerState' <- gyQueryUtxosAtAddress (ctxProviders ctx) user2SignerAddr (Just $ GYToken (mintingPolicyId user2SignerScript) "user3")
                localTokenState' <- gyQueryUtxosAtAddress (ctxProviders ctx) user2TokenAddr (Just $ GYToken (mintingPolicyId user2TokenScript) "user3")
                rewardUtxos <- gyQueryUtxosAtAddress (ctxProviders ctx) proveAddr (Just $ ctxIron ctx)

                user3BalanceBefore <- ctxQueryBalance ctx (ctxUser3 ctx)
                debug $ "user3 balance before: " ++ show user3BalanceBefore

                debug $ "user3 prove credentials and unlock reward"
                ctxRun ctx (ctxUser3 ctx) $ do

                  proveCredsSkeleton <- proveCredentials (utxosToUtxoRef global222TxRef) [utxosToUtxoRef localSignerState', utxosToUtxoRef localTokenState'] (utxosToHeadUtxo rewardUtxos) (GYInReference proveCredRefScrTxRef proveVal) "user3"
                  proveCredsTxBody  <- buildTxBody proveCredsSkeleton
                  signAndSubmitConfirmed_ proveCredsTxBody

                user3BalanceAfter <- ctxQueryBalance ctx (ctxUser3 ctx)
                debug $ "user3 balance after: " ++ show user3BalanceAfter

                assertBool "" True
              ]

utxosToUtxoRef :: GYUTxOs -> GYTxOutRef
utxosToUtxoRef utxos = utxoRef (utxosToHeadUtxo utxos)

utxosToHeadUtxo :: GYUTxOs -> GYUTxO
utxosToHeadUtxo utxos = head $ utxosToList utxos

mkRefScrTxOut :: GYAddress -> GYScript 'PlutusV3 -> GYTxOut 'PlutusV3
mkRefScrTxOut addr scr = GYTxOut
    { gyTxOutAddress = addr
    , gyTxOutValue = mempty
    , gyTxOutDatum = Nothing
    , gyTxOutRefS = Just (GYPlutusScript scr)
    }

getRefScrTxRef :: GYTxBody -> GYScript 'PlutusV3 -> IO GYTxOutRef
getRefScrTxRef txBody refScr = do
    let utxo = find (\u -> checkSomeScr (utxoRefScript u) refScr) (utxosToList $ txBodyUTxOs txBody)
    case utxo of
        Just u -> return (utxoRef u)
        Nothing -> error "getRefScrTxRef"

getRefScrTxRef' :: forall m. GYTxMonad m => GYTxBody -> GYScript 'PlutusV3 -> m GYTxOutRef
getRefScrTxRef' txBody refScr = do
    let utxo = find (\u -> checkSomeScr (utxoRefScript u) refScr) (utxosToList $ txBodyUTxOs txBody)
    case utxo of
        Just u -> pure (utxoRef u)
        Nothing -> error "getRefScrTxRef"

getAssetClassTxRef :: forall m. GYTxMonad m => GYTxBody -> GYAssetClass -> m GYUTxO
getAssetClassTxRef txBody assetClass = do
    let utxo = find (\u -> valueAssetClass (utxoValue u) assetClass > 0) (utxosToList $ txBodyUTxOs txBody)
    case utxo of
        Just u -> return u
        Nothing -> error "getAssetClassTxRef"

checkSomeScr :: (Maybe GYAnyScript) -> GYScript 'PlutusV3 -> Bool
checkSomeScr Nothing _ = False
checkSomeScr (Just (GYSimpleScript _)) _ = False
checkSomeScr (Just (GYPlutusScript sscr)) refScr = scriptPlutusHash refScr == scriptPlutusHash sscr