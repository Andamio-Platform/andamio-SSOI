module Andamio.Utility.API
    ( integerToWord32Safe
    , convertBech32AddressList
    , createCollateral
    , queryUtxoAtTxOutRef
    , queryUtxosAtTxOutRefs
    , tokensToAssetClass
    , csToGyScrHash
    , unsafeMPIdFromCs
    , queryUtxosFromAddress
    , queryUtxoFromAddressWithToken
    , queryUtxosFromAddressWithToken
    , queryTxOutRefWithRefScrAtAddress
    , mkRefScriptSkeleton
    ) where

import           Data.List         (find)
import           Data.Word         (Word32)
import           Prelude           (Maybe(..), Integer, map, fromIntegral, mempty,
                                   otherwise, maxBound, (<), (>), (||), (==), Either(..),
                                   IO, pure, ($), show, (++), error, maybe, (.), return)
import           PlutusLedgerApi.V3 (CurrencySymbol(..), ScriptHash(..))
import           GeniusYield.Types (GYAddressBech32, GYAddress, addressFromBech32,
                                   GYTxOutRef, GYTxOutRefCbor(..), GYUTxO(..), GYScriptHash,
                                   gyQueryUtxoAtTxOutRef, GYAssetClass(..), GYAnyScript(..),
                                   GYTokenName, GYMintingPolicyId, PlutusVersion(..), mintingPolicyIdFromCurrencySymbol,
                                   gyQueryUtxosAtAddress, utxosToList, GYScript, GYTxOut(..),
                                   validatorHashFromPlutus)
import           GeniusYield.TxBuilder (mustHaveOutput, GYTxQueryMonad, GYTxSkeleton)
import           Andamio.API.Context       (Ctx (..), ctxProviders)

integerToWord32Safe :: Integer -> Maybe Word32
integerToWord32Safe x
  | x < 0 || x > fromIntegral (maxBound :: Word32) = Nothing
  | otherwise = Just (fromIntegral x)

convertBech32AddressList :: [GYAddressBech32] -> [GYAddress]
convertBech32AddressList = map addressFromBech32

createCollateral :: Maybe GYTxOutRef -> Maybe GYTxOutRefCbor
createCollateral txRef =
    case txRef of
        Nothing -> Nothing
        Just txRef' -> Just (GYTxOutRefCbor txRef')

queryUtxoAtTxOutRef :: Ctx -> GYTxOutRef -> IO GYUTxO
queryUtxoAtTxOutRef ctx ref = do
    result <- gyQueryUtxoAtTxOutRef (ctxProviders ctx) ref
    maybe (error $ "UTxO not found for ref: " ++ show ref) pure result

queryUtxosAtTxOutRefs :: Ctx -> [GYTxOutRef] -> IO [GYUTxO]
queryUtxosAtTxOutRefs ctx txRefs = go txRefs []
  where
    go :: [GYTxOutRef] -> [GYUTxO] -> IO [GYUTxO]
    go [] counter = pure counter
    go (txRef:xs) counter = do
      result <- queryUtxoAtTxOutRef ctx txRef
      go xs (result:counter)

tokensToAssetClass :: [(GYMintingPolicyId, GYTokenName, Integer)] -> [(GYAssetClass, Integer)]
tokensToAssetClass ttd = map (\(pId, tn, am) -> (GYToken pId tn, am)) ttd

csToGyScrHash :: CurrencySymbol -> GYScriptHash
csToGyScrHash cs = case validatorHashFromPlutus (ScriptHash $ unCurrencySymbol cs) of
    Left e -> error $ "csToGyScrHash: " ++ show e
    Right hash -> hash

unsafeMPIdFromCs :: CurrencySymbol -> GYMintingPolicyId
unsafeMPIdFromCs cs = case mintingPolicyIdFromCurrencySymbol cs of
      Right pId -> pId
      Left e -> error ("not a currency symbol: " ++ show e)

queryUtxosFromAddress :: Ctx -> GYAddress -> IO [GYUTxO]
queryUtxosFromAddress ctx addr = do
    result <- gyQueryUtxosAtAddress (ctxProviders ctx) addr Nothing
    case utxosToList result of
        [] -> error $ "no utxos found at address: " ++ show addr
        txs -> pure txs

queryUtxoFromAddressWithToken :: Ctx -> GYAddress -> GYMintingPolicyId -> GYTokenName -> IO GYUTxO
queryUtxoFromAddressWithToken ctx addr cs tn = do
    result <- gyQueryUtxosAtAddress (ctxProviders ctx) addr (Just $ GYToken cs tn)
    case utxosToList result of
        [] -> error $ "no utxo found for token: " ++ show (cs, tn)
        [tx] -> pure tx
        _ -> error $ "more than one utxo found for token: " ++ show (cs, tn)

queryUtxosFromAddressWithToken :: Ctx -> GYAddress -> GYMintingPolicyId -> GYTokenName -> IO [GYUTxO]
queryUtxosFromAddressWithToken ctx addr cs tn = do
    result <- gyQueryUtxosAtAddress (ctxProviders ctx) addr (Just $ GYToken cs tn)
    case utxosToList result of
      --  [] -> error $ "no utxo found for token: " ++ show (cs, tn)
        tx -> pure tx

queryTxOutRefWithRefScrAtAddress :: Ctx -> GYAddress -> GYScript 'PlutusV3 -> IO GYTxOutRef
queryTxOutRefWithRefScrAtAddress ctx addr scr = do
    result <- gyQueryUtxosAtAddress (ctxProviders ctx) addr Nothing
    let refUtxo = find (\u -> utxoRefScript u == Just (GYPlutusScript scr)) (utxosToList result)
    maybe (error $ "UTxO not with reference script: " ++ show scr) (pure . utxoRef) refUtxo

mkRefScriptSkeleton :: GYTxQueryMonad m => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
mkRefScriptSkeleton addr scr = return $ mustHaveOutput GYTxOut
                                        { gyTxOutAddress = addr
                                        , gyTxOutValue = mempty
                                        , gyTxOutDatum = Nothing
                                        , gyTxOutRefS = Just (GYPlutusScript scr)
                                        }