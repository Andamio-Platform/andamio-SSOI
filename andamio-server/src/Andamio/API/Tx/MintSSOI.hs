module Andamio.API.Tx.MintSSOI
    ( handleMintSSOI
    , MintSSOITxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, String,
                                                   pure, ($), (++))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (mintSSOI)
import           Andamio.Utility.Tx                (findIndexUtxoByNewElement, indexTokenName', 
                                                   globalStateValidator, initIndexPolicy, indexScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral,
                                                   queryTxOutRefWithRefScrAtAddress, queryUtxosFromAddressWithToken)
import           Andamio.API.Tx.UnsignedTxResponse            (UnsignedTxResponse (..), unSignedTx')

data MintSSOITxRequest = MintSSOITxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   alias                ::  !String                -- ^ alias
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleMintSSOI :: Ctx -> AndamioConfig -> MintSSOITxRequest -> IO UnsignedTxResponse
handleMintSSOI ctx AndamioConfig{..} MintSSOITxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator
        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr

        indexRefTxRef <- queryTxOutRefWithRefScrAtAddress ctx (addressFromBech32 referenceAddress) indexScr
        indexAddr <- runQuery ctx $ scriptAddress indexScr
        indexUtxos' <- queryUtxosFromAddressWithToken ctx indexAddr (mintingPolicyId initPol) indexTokenName'
        indexUtxos <- queryUtxosFromAddressWithToken ctx indexAddr (mintingPolicyId indexScr) indexTokenName'

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mintSSOI indexRefTxRef indexScr (findIndexUtxoByNewElement (indexUtxos ++ indexUtxos') alias) alias globalStateAddr
        pure $ unSignedTx' txBody