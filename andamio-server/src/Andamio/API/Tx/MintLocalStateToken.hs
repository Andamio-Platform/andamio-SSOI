module Andamio.API.Tx.MintLocalStateToken
    ( handleMintLocalStateToken
    , MintLocalStateTokenTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           Data.String                       (fromString)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32,
                                                   GYInScript(..), GYMintScript(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String, (++))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (mintLSToken)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateTokenScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, 
                                                   queryUtxoFromAddressWithToken, queryTxOutRefWithRefScrAtAddress)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data MintLocalStateTokenTxRequest = MintLocalStateTokenTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   user222TxRef         ::  !GYTxOutRef            -- ^ issuer222 tx ref
    ,   userAlias            ::  !String                -- ^ alias to add credential
    ,   issuerAlias          ::  !String                -- ^ alias of issuer
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleMintLocalStateToken :: Ctx -> AndamioConfig -> MintLocalStateTokenTxRequest -> IO UnsignedTxResponse
handleMintLocalStateToken ctx AndamioConfig{..} MintLocalStateTokenTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateTokenScript issuerAlias (mintingPolicyId indexScr)

        localStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) localStateScr
        alias100Utxo <- queryUtxoFromAddressWithToken ctx globalStateAddr (mintingPolicyId indexScr) (fromString $ "100" ++ userAlias)
        globalStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) globalStateValidator
        localStateAddr <- runQuery ctx $ scriptAddress localStateScr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mintLSToken user222TxRef alias100Utxo (GYInReference globalStateRefTxRef globalStateValidator) (GYMintReference localStateRefTxRef localStateScr) localStateAddr
        pure $ unSignedTx' txBody