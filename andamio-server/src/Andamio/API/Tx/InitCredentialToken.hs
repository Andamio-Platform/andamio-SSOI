module Andamio.API.Tx.InitCredentialToken
    ( handleInitCredentialToken
    , InitCredentialTokenTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), String)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..),
                                                   runTxI, runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateTokenScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral, mkRefScriptSkeleton)
import           Andamio.API.Tx.UnsignedTxResponse            (UnsignedTxResponse (..), unSignedTx')

data InitCredentialTokenTxRequest = InitCredentialTokenTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   issuerAlias          ::  !String                -- ^ issuer alias without prefix
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleInitCredentialToken :: Ctx -> AndamioConfig -> InitCredentialTokenTxRequest -> IO UnsignedTxResponse
handleInitCredentialToken ctx AndamioConfig{..} InitCredentialTokenTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator
        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mkRefScriptSkeleton (addressFromBech32 referenceAddress) (localStateTokenScript issuerAlias (mintingPolicyId indexScr))
        pure $ unSignedTx' txBody