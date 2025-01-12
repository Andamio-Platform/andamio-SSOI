module Andamio.Utility.Tx
  ( outDatToDat
  , outDatToBd
  , unsafeGYOtDatumToData
  , bbsToGyTn
  , bbs2ToGyTn
  , indexTokenName'
  , initIndexPolicy
  , indexScript
  , globalStateValidator
  , localStateSignerScript
  , localStateTokenScript
  , proveCredsScript
  , mustHaveIndexOutput
  , gyScriptToPlutusCs
  , createProveCredsDatum
  , findIndexUtxoByNewElement
  ) where

import           Data.List                  (sort, find)
import           Data.Maybe                 (fromJust)
import           Prelude                    (error, Maybe(..), ($), otherwise, (.), String, Integer, map, (==))
import           GeniusYield.Types
import           GeniusYield.TxBuilder
import           PlutusTx                   (unsafeFromBuiltinData, UnsafeFromData, toBuiltinData)
import           PlutusTx.Prelude           (BuiltinByteString, appendByteString, BuiltinData)
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import           PlutusLedgerApi.V3         (TokenName (..), TxOutRef(..), TxId(..), CurrencySymbol(..))
import qualified PlutusLedgerApi.V2   as V2 (TxOutRef(..), TxId(..))
import           Andamio.Index.Compiled             (indexTokenName, indexScriptsSerialised,
                                            IndexParams(..), initIndexPolicySerialised)
import           Andamio.GlobalState.Compiled       (globalStateValidatorSerialised)
import           Andamio.LocalState.Compiled        (localStateSignerSerialised, SignerParams(..),
                                            localStateTokenSerialised, TokenParams(..))
import           Andamio.ProveCreds.Compiled        (proveCredsValidatorSerialised, ProveCredsDatum(..))

-- ^ ##### Scripts #####

initIndexPolicy :: GYTxOutRef -> GYScript 'PlutusV3
initIndexPolicy txOutRef = scriptFromSerialisedScript $ initIndexPolicySerialised $ toBuiltinData $ txOutRefV2ToV3 $ txOutRefToPlutus txOutRef

indexScript :: GYMintingPolicyId -> GYAddress -> GYScript 'PlutusV3
indexScript pId addr = scriptFromSerialisedScript $ indexScriptsSerialised ip
  where
    ip :: IndexParams
    ip = IndexParams 
              (mintingPolIdToBuiltinCs pId) 
              (toBuiltinData $ addressToPlutus addr)

globalStateValidator :: GYScript 'PlutusV3
globalStateValidator = scriptFromSerialisedScript globalStateValidatorSerialised

localStateSignerScript :: GYPubKeyHash -> GYMintingPolicyId -> GYScript 'PlutusV3
localStateSignerScript ownerPkh gsCs = scriptFromSerialisedScript $ localStateSignerSerialised sp
  where
    sp :: SignerParams
    sp = SignerParams
              (toBuiltinData $ pubKeyHashToPlutus ownerPkh)
              (mintingPolIdToBuiltinCs gsCs)

localStateTokenScript :: String -> GYMintingPolicyId -> GYScript 'PlutusV3
localStateTokenScript ownerAlias gsCs = scriptFromSerialisedScript $ localStateTokenSerialised tp
  where
    tp :: TokenParams
    tp = TokenParams
              (stringToBuiltinByteString ownerAlias)
              (mintingPolIdToBuiltinCs gsCs)

proveCredsScript :: GYMintingPolicyId -> GYScript 'PlutusV3
proveCredsScript gsCs = scriptFromSerialisedScript $ proveCredsValidatorSerialised $ mintingPolIdToBuiltinCs gsCs

-- ^ ##### Datum #####

outDatToDat :: GYOutDatum -> GYDatum 
outDatToDat (GYOutDatumInline gyDat) = gyDat
outDatToDat _ = error "wrong datum format"

outDatToBd :: GYOutDatum -> BuiltinData
outDatToBd (GYOutDatumInline dat) = datumToPlutus' dat
outDatToBd _ = error "wrong datum format"

unsafeGYOtDatumToData :: forall dat. (UnsafeFromData dat) => GYOutDatum -> dat
unsafeGYOtDatumToData (GYOutDatumInline bd) = unsafeFromBuiltinData @dat (datumToPlutus' bd)
unsafeGYOtDatumToData _ = error "wrong datum format"

createProveCredsDatum :: [(GYMintingPolicyId, [String])] -> [(GYMintingPolicyId, Integer)] -> ProveCredsDatum
createProveCredsDatum strCreds intCreds = ProveCredsDatum
                                                { getBbsCreds = map (\(s, n) -> (mintingPolicyIdToCurrencySymbol s, map stringToBuiltinByteString n)) strCreds
                                                , getIntCreds = map (\(s, n) -> (mintingPolicyIdToCurrencySymbol s, n)) intCreds 
                                                }

-- ^ ##### Value #####

bbsToGyTn :: BuiltinByteString -> GYTokenName
bbsToGyTn bbs1 
  | Just gytn <- tokenNameFromPlutus $ TokenName bbs1 = gytn
  | otherwise = error "token name not possible"

bbs2ToGyTn :: BuiltinByteString -> BuiltinByteString -> GYTokenName
bbs2ToGyTn bbs1 bbs2 = bbsToGyTn (appendByteString bbs1 bbs2)

indexTokenName' :: GYTokenName
indexTokenName' = fromJust $ tokenNameFromPlutus indexTokenName

mintingPolIdToBuiltinCs :: GYMintingPolicyId -> BuiltinData
mintingPolIdToBuiltinCs = toBuiltinData . mintingPolicyIdToCurrencySymbol

txOutRefV2ToV3 :: V2.TxOutRef -> TxOutRef
txOutRefV2ToV3 (V2.TxOutRef (V2.TxId txId) txIx) = TxOutRef (TxId txId) txIx

gyScriptToPlutusCs :: GYScript 'PlutusV3 -> CurrencySymbol
gyScriptToPlutusCs = mintingPolicyIdToCurrencySymbol . mintingPolicyId

-- ^ ##### Tx Skeleton #####

mustHaveIndexOutput :: GYAddress -> GYValue -> BuiltinByteString -> BuiltinByteString -> GYTxSkeleton 'PlutusV3
mustHaveIndexOutput addr val s1 s2 = mustHaveOutput $ GYTxOut 
                                      { gyTxOutAddress = addr
                                      , gyTxOutValue = val
                                      , gyTxOutDatum = Just (datumFromPlutusData (s1, s2), GYTxOutUseInlineDatum)
                                      , gyTxOutRefS = Nothing
                                      }

-- ^ ##### UTxO #####

-- ^ if String between BuiltinByteString then return GYUTxO
findIndexUtxoByNewElement :: [GYUTxO] -> String -> GYUTxO
findIndexUtxoByNewElement utxos new
  | Just utxo <- find (\(GYUTxO _ _ _ dat _) -> 
                            let (t, n) = getThisAndNext dat in 
                            [t, stringToBuiltinByteString new, n] == sort [t, stringToBuiltinByteString new, n] ) utxos = 
                              utxo
  | otherwise = error "findIndexUtxoDat"
  where
    getThisAndNext :: GYOutDatum -> (BuiltinByteString, BuiltinByteString)
    getThisAndNext (GYOutDatumInline gydat) = unsafeFromBuiltinData @(BuiltinByteString, BuiltinByteString) (datumToPlutus' gydat)
    getThisAndNext _ = error "wrong index datum"