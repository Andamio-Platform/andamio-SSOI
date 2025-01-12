module Andamio.API.Query.GetIndexScriptData
    ( handleGetIndexScriptData
    , GetIndexScriptDataResponse (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (ToJSON)
import           GeniusYield.Types                 (mintingPolicyId,
                                                   GYAddressBech32, GYMintingPolicyId,
                                                   addressToBech32)
import           GeniusYield.TxBuilder             (scriptAddress)
import           Prelude                           (IO, Show, pure, ($))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript)

data GetIndexScriptDataResponse = GetIndexScriptDataResponse
    {   initIndexPolicyId    ::  !GYMintingPolicyId     -- ^ used wallet addresses
    ,   indexScriptHash      ::  !GYMintingPolicyId       -- ^ change address
    ,   indexAddress         ::  !GYAddressBech32    -- ^ maybe collateral tx ref
    } deriving (Show, Generic, ToJSON, Swagger.ToSchema)

handleGetIndexScriptData :: Ctx -> AndamioConfig -> IO GetIndexScriptDataResponse
handleGetIndexScriptData ctx AndamioConfig{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
        indexAddr <- runQuery ctx $ scriptAddress indexScr

        pure $ GetIndexScriptDataResponse (mintingPolicyId initPol) (mintingPolicyId indexScr) (addressToBech32 indexAddr)