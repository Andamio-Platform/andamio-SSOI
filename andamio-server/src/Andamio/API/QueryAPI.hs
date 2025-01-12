module Andamio.API.QueryAPI
      ( QueryAPI
      , handleQueryAPI
      ) where

import           Andamio.API.Context                    (Ctx, AndamioConfig)
import           Prelude                                (IO)
import           Servant                 

import           Andamio.API.Query.GetIndexScriptData (GetIndexScriptDataResponse, handleGetIndexScriptData)
import           Andamio.API.Query.LocalStateScriptDataResponse (LocalStateScriptDataResponse)
import           Andamio.API.Query.GetLSSignerScriptData (GetLSSignerScriptDataRequest, handleGetLSSignerScriptData)
import           Andamio.API.Query.GetLSTokenScriptData (GetLSTokenScriptDataRequest, handleGetLSTokenScriptData)
import           Andamio.API.Query.GetCredentialsByAlias (GetCredentialsByAliasRequest, handleGetCredentialsByAlias,
                                                         GetCredentialsByAliasResponse)
import           Andamio.API.Query.GetPossibleRewardTxRefs (GetPossibleRewardTxRefsRequest, handleGetPossibleRewardTxRefs, 
                                                              GetPossibleRewardTxRefsResponse)


type QueryAPI = 
         Description "Handles the request to get index script data."
      :> "get-index-script-data"
      :> Get     '[JSON] GetIndexScriptDataResponse
   :<|> Description "Handles the request to get local state script signer data."
      :>  "get-local-state-signer-data"
      :>  ReqBody '[JSON] GetLSSignerScriptDataRequest
      :>  Post    '[JSON] LocalStateScriptDataResponse
   :<|> Description "Handles the request to get local state script token data."
      :>  "get-local-state-token-data"
      :>  ReqBody '[JSON] GetLSTokenScriptDataRequest
      :>  Post    '[JSON] LocalStateScriptDataResponse
   :<|> Description "Handles the request to get all tx refs possible to be unlocked from prove validator for a given global token alias."
      :>  "get-possible-reward-tx-refs"
      :>  ReqBody '[JSON] GetPossibleRewardTxRefsRequest
      :>  Post    '[JSON] GetPossibleRewardTxRefsResponse
   :<|> Description "Handles the request to get all credentials for a specific alias."
      :>  "get-credentials-by-alias"
      :>  ReqBody '[JSON] GetCredentialsByAliasRequest
      :>  Post    '[JSON] GetCredentialsByAliasResponse

handleQueryAPI :: Ctx -> AndamioConfig -> ServerT QueryAPI IO
handleQueryAPI ctx andamioConf = handleGetIndexScriptData ctx andamioConf
                            :<|> handleGetLSSignerScriptData ctx andamioConf
                            :<|> handleGetLSTokenScriptData ctx andamioConf
                            :<|> handleGetPossibleRewardTxRefs ctx andamioConf
                            :<|> handleGetCredentialsByAlias ctx andamioConf