module Andamio.API.Tx.AddCredentialSigner
    ( handleAddCredentialSigner
    , AddCredentialSignerTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Maybe                        (fromJust)
import           Data.Aeson                        (FromJSON)
import           Data.String                       (fromString)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32,
                                                   addressToPubKeyHash, GYInScript(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (addCredSigner)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateSignerScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, 
                                                   queryUtxoFromAddressWithToken, queryTxOutRefWithRefScrAtAddress)
import           Andamio.API.Tx.UnsignedTxResponse            (UnsignedTxResponse (..), unSignedTx')

data AddCredentialSignerTxRequest = AddCredentialSignerTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   credential           ::  !String                -- ^ credential to add
    ,   aliasToCred          ::  !String                -- ^ alias to add credential
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleAddCredentialSigner :: Ctx -> AndamioConfig -> AddCredentialSignerTxRequest -> IO UnsignedTxResponse
handleAddCredentialSigner ctx AndamioConfig{..} AddCredentialSignerTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateSignerScript (fromJust $ addressToPubKeyHash $ addressFromBech32 changeAddress) (mintingPolicyId indexScr)

        localStateAddr <- runQuery ctx $ scriptAddress localStateScr
        localStateUtxo <- queryUtxoFromAddressWithToken ctx localStateAddr (mintingPolicyId localStateScr) (fromString aliasToCred)
        localStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) localStateScr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ addCredSigner (addressFromBech32 changeAddress) localStateUtxo (GYInReference localStateRefTxRef localStateScr) aliasToCred credential
        pure $ unSignedTx' txBody