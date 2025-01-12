module Andamio.API.Tx.UnsignedTxResponse (
    UnsignedTxResponse (..),
    unSignedTx'
) where

import           GHC.Generics                   (Generic)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Swagger        as Swagger (ToSchema)
import qualified Data.Text           as T       (pack, Text)
import           GeniusYield.Types              (GYTxBody, txToHex, unsignedTx)
import           Prelude                        (Show, ($))


newtype UnsignedTxResponse = UnsignedTxResponse
    {
        unsignedTxCBOR :: T.Text
    }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema)

unSignedTx' :: GYTxBody -> UnsignedTxResponse
unSignedTx' txBody = UnsignedTxResponse
    {
        unsignedTxCBOR  = T.pack $ txToHex $ unsignedTx txBody
    }
