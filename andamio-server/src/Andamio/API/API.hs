module Andamio.API.API
      ( SSOIAPI
      , andamioAPI
      , apiSwagger
      , apiServer) where

import           Andamio.API.Context                    (Ctx, AndamioConfig)
import           Data.Swagger                           (Swagger)
import           Prelude                                (IO)
import           Servant                 
import           Servant.Swagger                        (toSwagger)

import           Andamio.API.TxAPI                      (TxAPI, handleTxV0API)
import           Andamio.API.QueryAPI                   (QueryAPI, handleQueryAPI)

type SSOIAPI = 
      Summary "Andamio SSOI API"
      :> Description "Endpoints to build transactions."
      :> "tx" :> TxAPI
   :<|> Description "Endpointy to query data"
      :> "query" :> QueryAPI

andamioAPI :: Proxy SSOIAPI
andamioAPI = Proxy

apiSwagger  :: Swagger
apiSwagger  = toSwagger andamioAPI

apiServer :: Ctx -> AndamioConfig ->  ServerT SSOIAPI IO
apiServer ctx andamioConf = handleTxV0API ctx andamioConf
                       :<|> handleQueryAPI ctx andamioConf