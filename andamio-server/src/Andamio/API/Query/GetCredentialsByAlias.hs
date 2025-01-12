module Andamio.API.Query.GetCredentialsByAlias
    ( handleGetCredentialsByAlias
    , GetCredentialsByAliasRequest (..)
    , GetCredentialsByAliasResponse (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (ToJSON, FromJSON)
import           Data.String                       (fromString)
import           PlutusLedgerApi.V3                (CurrencySymbol(..), BuiltinData, unsafeFromBuiltinData, fromBuiltinData)
import           PlutusTx.Builtins.Internal            (BuiltinByteString(..))
import           Data.Text                  as Text    (unpack)
import           Data.Text.Encoding         as Text    (decodeUtf8)
import           Data.ByteString.Base16     as Base16  (encode)
import           GeniusYield.Types                 (mintingPolicyId, addressFromScriptHash,
                                                   GYUTxO(..), GYMintingPolicyId)
import           GeniusYield.TxBuilder             (scriptAddress)
import           GeniusYield.GYConfig              (GYCoreConfig(..))
import           Prelude                           (IO, Show, pure, ($), Integer,  String, (++), Maybe(..), map)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.GlobalState.OnChain.GlobalStateDatum (GlobalStateDatum(..))
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript, unsafeGYOtDatumToData, outDatToBd)
import           Andamio.Utility.API               (unsafeMPIdFromCs, csToGyScrHash, queryUtxoFromAddressWithToken)

data GetCredentialsByAliasRequest = GetCredentialsByAliasRequest
    {   alias   :: !String
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data GetCredentialsByAliasResponse = GetCredentialsByAliasResponse
    {   requiredIntCredentials  ::  ![(GYMintingPolicyId, Integer)]
    ,   requiredStrCredentials  ::  ![(GYMintingPolicyId, [String])]
    } deriving (Show, Generic, ToJSON, Swagger.ToSchema)

handleGetCredentialsByAlias :: Ctx -> AndamioConfig -> GetCredentialsByAliasRequest -> IO GetCredentialsByAliasResponse
handleGetCredentialsByAlias ctx AndamioConfig{..} GetCredentialsByAliasRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr

        alias100Utxo <- queryUtxoFromAddressWithToken ctx globalStateAddr (mintingPolicyId indexScr) (fromString $ "100" ++ alias)
        allCredsDatum <- queryAllCredentials (localStateCss $ unsafeGYOtDatumToData $ utxoOutDatum alias100Utxo) (GetCredentialsByAliasResponse [] [])

        pure allCredsDatum

        where

          queryAllCredentials :: [CurrencySymbol] -> GetCredentialsByAliasResponse -> IO GetCredentialsByAliasResponse
          queryAllCredentials [] response = pure response
          queryAllCredentials (cs:xs) response = do
            result <- queryUtxoFromAddressWithToken ctx (addressFromScriptHash (cfgNetworkId $ ctxCoreCfg ctx) $ csToGyScrHash cs) (unsafeMPIdFromCs cs) (fromString alias)
            queryAllCredentials xs (createResponse response (unsafeMPIdFromCs cs) $ outDatToBd $ utxoOutDatum result)

          createResponse :: GetCredentialsByAliasResponse -> GYMintingPolicyId -> BuiltinData -> GetCredentialsByAliasResponse
          createResponse resp polId bd = case fromBuiltinData @Integer bd of
            Just int -> resp{requiredIntCredentials = (polId, int):requiredIntCredentials resp}
            Nothing  -> resp{requiredStrCredentials = (polId, map fromBbs $ unsafeFromBuiltinData @[BuiltinByteString] bd):requiredStrCredentials resp}
          
          fromBbs :: BuiltinByteString -> String
          fromBbs (BuiltinByteString bs) = Text.unpack $ Text.decodeUtf8 $ Base16.encode bs