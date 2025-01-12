module Andamio.API.Query.GetPossibleRewardTxRefs
    ( handleGetPossibleRewardTxRefs
    , GetPossibleRewardTxRefsRequest (..)
    , GetPossibleRewardTxRefsResponse (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (ToJSON, FromJSON)
import           Data.String                       (fromString)
import           PlutusLedgerApi.V3                (CurrencySymbol(..), BuiltinByteString)
import           GeniusYield.Types                 (mintingPolicyId, addressFromScriptHash,
                                                   GYTxOutRef, GYUTxO(..))
import           GeniusYield.TxBuilder             (scriptAddress)
import           GeniusYield.GYConfig              (GYCoreConfig(..))
import           Prelude                           (IO, Show, pure, ($), Integer, Bool(..), String, elem, all, (<=), (&&))
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.ProveCreds.OnChain.ProveCredsDatum (ProveCredsDatum(..))
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript, unsafeGYOtDatumToData, proveCredsScript)
import           Andamio.Utility.API               (queryUtxosFromAddress, unsafeMPIdFromCs, csToGyScrHash, queryUtxoFromAddressWithToken)

data GetPossibleRewardTxRefsRequest = GetPossibleRewardTxRefsRequest
    {   alias   :: !String
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data GetPossibleRewardTxRefsResponse = GetPossibleRewardTxRefsResponse
    {   rewardTxRefs   :: ![GYTxOutRef]
    } deriving (Show, Generic, ToJSON, Swagger.ToSchema)

handleGetPossibleRewardTxRefs :: Ctx -> AndamioConfig -> GetPossibleRewardTxRefsRequest -> IO GetPossibleRewardTxRefsResponse
handleGetPossibleRewardTxRefs ctx AndamioConfig{..} GetPossibleRewardTxRefsRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator

        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            proveCredScr = proveCredsScript (mintingPolicyId indexScr)
        proveCredAddr <- runQuery ctx $ scriptAddress proveCredScr

        rewardUtxos <- queryUtxosFromAddress ctx proveCredAddr
        possibleTxRefs <- queryAllPossibleTxRefs rewardUtxos []

        pure (GetPossibleRewardTxRefsResponse possibleTxRefs)

        where
            queryAllPossibleTxRefs :: [GYUTxO] -> [GYTxOutRef] -> IO [GYTxOutRef]
            queryAllPossibleTxRefs [] txRefs = pure txRefs
            queryAllPossibleTxRefs (x:xs) txRefs = do
                let pcDat = (unsafeGYOtDatumToData $ utxoOutDatum x)
                intBool <- goInt (getIntCreds pcDat)
                strBool <- goStr (getBbsCreds pcDat)
                if intBool && strBool then queryAllPossibleTxRefs xs (utxoRef x:txRefs) else queryAllPossibleTxRefs xs txRefs
              where
                goInt :: [(CurrencySymbol, Integer)] -> IO Bool
                goInt [] = pure True
                goInt ((cs, int):ys) = do
                  result <- queryUtxoFromAddressWithToken ctx (addressFromScriptHash (cfgNetworkId $ ctxCoreCfg ctx) $ csToGyScrHash cs) (unsafeMPIdFromCs cs) (fromString alias)
                  if int <= unsafeGYOtDatumToData @Integer (utxoOutDatum result) then goInt ys else pure False
                
                goStr :: [(CurrencySymbol, [BuiltinByteString])] -> IO Bool
                goStr [] = pure True
                goStr ((cs, bbsL):ys) = do
                  result <- queryUtxoFromAddressWithToken ctx (addressFromScriptHash (cfgNetworkId $ ctxCoreCfg ctx) $ csToGyScrHash cs) (unsafeMPIdFromCs cs) (fromString alias)
                  if all (`elem` unsafeGYOtDatumToData @[BuiltinByteString] (utxoOutDatum result)) bbsL then goStr ys else pure False