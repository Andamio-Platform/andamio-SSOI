module Andamio.API.Tx.ProveCredentials
    ( handleProveCredentials
    , ProveCredentialsTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           Data.String                       (fromString)
import           PlutusLedgerApi.V3                (CurrencySymbol(..))
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId, GYUTxO(..),
                                                   GYTxOutRef, GYAddressBech32, addressFromScriptHash,
                                                   GYInScript(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           GeniusYield.GYConfig              (GYCoreConfig(..))
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String, fst, map, (++))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.ProveCreds.OnChain.ProveCredsDatum (ProveCredsDatum(..))
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (proveCredentials)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   unsafeGYOtDatumToData, proveCredsScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, 
                                                   queryUtxoAtTxOutRef, unsafeMPIdFromCs, csToGyScrHash,
                                                   queryUtxoFromAddressWithToken, queryTxOutRefWithRefScrAtAddress)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data ProveCredentialsTxRequest = ProveCredentialsTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   user222TxRef         ::  !GYTxOutRef            -- ^ user global 222 token tx ref
    ,   rewardTxRef          ::  !GYTxOutRef            -- ^ prove credential reward tx ref, get rewards if can prove credentials
    ,   alias                ::  !String                -- ^ alias who wants to unlock
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleProveCredentials :: Ctx -> AndamioConfig -> ProveCredentialsTxRequest -> IO UnsignedTxResponse
handleProveCredentials ctx AndamioConfig{..} ProveCredentialsTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            proveCredScr = proveCredsScript (mintingPolicyId indexScr)

        proveCredRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) proveCredScr
        rewardUtxo <- queryUtxoAtTxOutRef ctx rewardTxRef
        proveCredsTxRefs <- queryAllProveTxRefs (unsafeGYOtDatumToData $ utxoOutDatum rewardUtxo)

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ proveCredentials user222TxRef proveCredsTxRefs rewardUtxo (GYInReference proveCredRefTxRef proveCredScr) alias
        pure $ unSignedTx' txBody

        where
            
            queryAllProveTxRefs :: ProveCredsDatum -> IO [GYTxOutRef]
            queryAllProveTxRefs pcDat = do
                let credCsNeeded = map fst (getBbsCreds pcDat) ++ map fst (getIntCreds pcDat)
                txRefs <- go credCsNeeded []
                pure txRefs
              where
                go :: [CurrencySymbol] -> [GYTxOutRef] -> IO [GYTxOutRef]
                go [] counter = pure counter
                go (cs:xs) counter = do
                  result <- queryUtxoFromAddressWithToken ctx (addressFromScriptHash (cfgNetworkId $ ctxCoreCfg ctx) $ csToGyScrHash cs) (unsafeMPIdFromCs cs) (fromString alias)
                  go xs (utxoRef result:counter)

    
