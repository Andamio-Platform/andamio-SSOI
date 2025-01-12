module Andamio.API.Query.LocalStateScriptDataResponse (
    LocalStateScriptDataResponse (..)
    ) where

import           GHC.Generics                   (Generic)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Swagger        as Swagger (ToSchema)
import           GeniusYield.Types              (GYMintingPolicyId)
import           Prelude                        (Show)


newtype LocalStateScriptDataResponse = LocalStateScriptDataResponse
    {
        scriptHash :: GYMintingPolicyId
    }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema)