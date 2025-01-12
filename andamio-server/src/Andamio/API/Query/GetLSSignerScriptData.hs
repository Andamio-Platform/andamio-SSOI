module Andamio.API.Query.GetLSSignerScriptData
    ( handleGetLSSignerScriptData
    , GetLSSignerScriptDataRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Maybe                        (fromJust)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (mintingPolicyId, addressToPubKeyHash,
                                                   GYAddressBech32, addressFromBech32)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (IO, Show, pure, ($))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateSignerScript)
import           Andamio.API.Query.LocalStateScriptDataResponse (LocalStateScriptDataResponse(..))

data GetLSSignerScriptDataRequest = GetLSSignerScriptDataRequest
    {   ownerAddress          ::  !GYAddressBech32
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleGetLSSignerScriptData :: Ctx -> AndamioConfig -> GetLSSignerScriptDataRequest -> IO LocalStateScriptDataResponse
handleGetLSSignerScriptData ctx AndamioConfig{..} GetLSSignerScriptDataRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateSignerScript (fromJust $ addressToPubKeyHash $ addressFromBech32 ownerAddress) (mintingPolicyId indexScr)

        pure $ LocalStateScriptDataResponse (mintingPolicyId localStateScr)