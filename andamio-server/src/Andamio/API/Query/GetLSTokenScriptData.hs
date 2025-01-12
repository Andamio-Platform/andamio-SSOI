module Andamio.API.Query.GetLSTokenScriptData
    ( handleGetLSTokenScriptData
    , GetLSTokenScriptDataRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (mintingPolicyId)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (IO, Show, pure, ($), String)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   localStateTokenScript)
import           Andamio.API.Query.LocalStateScriptDataResponse (LocalStateScriptDataResponse(..))

data GetLSTokenScriptDataRequest = GetLSTokenScriptDataRequest
    {   ownerAlias          ::  !String
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleGetLSTokenScriptData :: Ctx -> AndamioConfig -> GetLSTokenScriptDataRequest -> IO LocalStateScriptDataResponse
handleGetLSTokenScriptData ctx AndamioConfig{..} GetLSTokenScriptDataRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            localStateScr = localStateTokenScript ownerAlias (mintingPolicyId indexScr)

        pure $ LocalStateScriptDataResponse (mintingPolicyId localStateScr)