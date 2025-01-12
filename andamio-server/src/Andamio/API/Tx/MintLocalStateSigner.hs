module Andamio.API.Tx.MintLocalStateSigner
    ( handleMintLocalStateSigner
    , MintLocalStateSignerTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           Data.String                       (fromString)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32,
                                                   GYInScript(..), GYPubKeyHash, GYMintScript(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String, (++))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (mintLSSigner)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateSignerScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, 
                                                   queryUtxoFromAddressWithToken, queryTxOutRefWithRefScrAtAddress)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data MintLocalStateSignerTxRequest = MintLocalStateSignerTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   user222TxRef         ::  !GYTxOutRef            -- ^ issuer222 tx ref
    ,   userAlias            ::  !String                -- ^ alias to add credential
    ,   issuerPkh            ::  !GYPubKeyHash          -- ^ public key hash of issuer
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleMintLocalStateSigner :: Ctx -> AndamioConfig -> MintLocalStateSignerTxRequest -> IO UnsignedTxResponse
handleMintLocalStateSigner ctx AndamioConfig{..} MintLocalStateSignerTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateSignerScript issuerPkh (mintingPolicyId indexScr)

        localStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) localStateScr
        alias100Utxo <- queryUtxoFromAddressWithToken ctx globalStateAddr (mintingPolicyId indexScr) (fromString $ "100" ++ userAlias)
        globalStateRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) globalStateValidator
        localStateAddr <- runQuery ctx $ scriptAddress localStateScr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mintLSSigner user222TxRef alias100Utxo (GYInReference globalStateRefTxRef globalStateValidator) (GYMintReference localStateRefTxRef localStateScr) localStateAddr
        pure $ unSignedTx' txBody