module Andamio.API.Tx.InitCredentialSigner
    ( handleInitCredentialSigner
    , InitCredentialSignerTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Maybe                        (fromJust)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32,
                                                   addressToPubKeyHash)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateSignerScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, mkRefScriptSkeleton)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data InitCredentialSignerTxRequest = InitCredentialSignerTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleInitCredentialSigner :: Ctx -> AndamioConfig -> InitCredentialSignerTxRequest -> IO UnsignedTxResponse
handleInitCredentialSigner ctx AndamioConfig{..} InitCredentialSignerTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator
        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mkRefScriptSkeleton (addressFromBech32 referenceAddress) (localStateSignerScript (fromJust $ addressToPubKeyHash $ addressFromBech32 changeAddress) (mintingPolicyId indexScr))
        pure $ unSignedTx' txBody