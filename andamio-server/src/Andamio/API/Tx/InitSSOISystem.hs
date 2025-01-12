module Andamio.API.Tx.InitSSOISystem
    ( handleInitSSOISystem
    , InitSSOISystemTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId,
                                                   GYTxOutRef, GYAddressBech32)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (Maybe(..), IO, Show, pure, ($))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Tx                        (initIndexGs)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, 
                                                   indexScript, proveCredsScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data InitSSOISystemTxRequest = InitSSOISystemTxRequest
    {   usedAddresses        ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress        ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef      ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleInitSSOISystem :: Ctx -> AndamioConfig -> InitSSOISystemTxRequest -> IO UnsignedTxResponse
handleInitSSOISystem ctx AndamioConfig{..} InitSSOISystemTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator
        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            proveCredScr = proveCredsScript (mintingPolicyId indexScr)
        
        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ initIndexGs seedTxRef initPol indexScr globalStateValidator proveCredScr (addressFromBech32 referenceAddress)
        pure $ unSignedTx' txBody