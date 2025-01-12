module Andamio.API.Tx.AddCredentialToken
    ( handleAddCredentialToken
    , AddCredentialTokenTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           Data.String                       (fromString)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32,
                                                   GYInScript(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (addCredToken)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateTokenScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, 
                                                   queryUtxoFromAddressWithToken, queryTxOutRefWithRefScrAtAddress)
import           Andamio.API.Tx.UnsignedTxResponse            (UnsignedTxResponse (..), unSignedTx')

data AddCredentialTokenTxRequest = AddCredentialTokenTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   issuer222TxRef       ::  !GYTxOutRef            -- ^ issuer222 tx ref
    ,   aliasToCred          ::  !String                -- ^ alias to add credential
    ,   issuerAlias          ::  !String                -- ^ issuer alias without prefix
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleAddCredentialToken :: Ctx -> AndamioConfig -> AddCredentialTokenTxRequest -> IO UnsignedTxResponse
handleAddCredentialToken ctx AndamioConfig{..} AddCredentialTokenTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateTokenScript issuerAlias (mintingPolicyId indexScr)

        localStateAddr <- runQuery ctx $ scriptAddress localStateScr
        localStateUtxo <- queryUtxoFromAddressWithToken ctx localStateAddr (mintingPolicyId localStateScr) (fromString aliasToCred)
        localStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) localStateScr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ addCredToken issuer222TxRef localStateUtxo (GYInReference localStateRefTxRef localStateScr) aliasToCred
        pure $ unSignedTx' txBody