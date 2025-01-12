module Andamio.Index.OnChain.IndexScripts.MintingScript
                    ( validateMinting
                    , indexTokenNameBd
                    ) where


import PlutusLedgerApi.V3                                    (unsafeFromBuiltinData, toBuiltinData, CurrencySymbol(..))
import PlutusTx.Prelude                               as PPr (Bool(..), fst, snd, BuiltinByteString, 
                                                             (==), otherwise, (&&), ($), error, 
                                                             (||), (<), BuiltinData)
import PlutusTx.Builtins.Internal                     as BI  (head, tail, BuiltinList(..), BuiltinPair, 
                                                             unsafeDataAsMap, mkI, unsafeDataAsB)
import PlutusTx.Builtins                              as B   (null)

import Andamio.Utility.OnChain.TxInInfo                      (lazyTxInInfoTxOutInlineDatBd,
                                                             lazyTxInInfoValueMapBd)
import Andamio.Utility.OnChain.Value                         (singleTokenInValueBd, create222Tn, create100Tn,
                                                             lengthListTupleBd, tnAmBdMapByCsFromValueBd,
                                                             builtinsElemMapBd, lengthBuiltinValueIf1Tn)
import Andamio.Utility.OnChain.Datum                         (mkInlineDatumBuiltin, nothingBd)
import Andamio.Utility.OnChain.TxOut                         (lazyTxOutValueMapBd, lazyTxOutAddrBd,
                                                             lazyTxOutReferenceScriptBd,
                                                             findLazyTxOutBySingleToken,
                                                             lazyTxOutDatumBd, lazyTxOutAddrBd)
import Andamio.Utility.OnChain.Address                       (addressFromScriptHashNoStakeBd)

import Andamio.Index.OnChain.InitIndexPolicy                 (indexTokenNameBd)
import Andamio.Index.OnChain.IndexScripts.IndexParams        (IndexParams(..))
import Andamio.GlobalState.OnChain.GlobalStateDatum          (GlobalStateDatum(..))

-- ^ Minting script handling logic for uniqueness of token names using a linked list. 
-- ^ Each utxo has same token attached and a builtin byte string tuple as datum representing the data and the next element.
-- ^ To add a new element (mint a new unique name) the datum tuple is splitted.

{-# INLINEABLE validateMinting #-}
validateMinting :: IndexParams -> BuiltinByteString -> BuiltinData -> BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> Bool
validateMinting IndexParams{..} newElement ownSymbolBd txInfoMintBd txInfoInputsBd txInfoOutputsBd =

        let dat = getOwnInputDatum txInfoInputsBd -- ^ own input builtin byte string tuple
            thisPolicyMintTnAm = tnAmBdMapByCsFromValueBd (BI.unsafeDataAsMap txInfoMintBd) ownSymbolBd -- ^ get (token name, amount) minted with own currency symbol
            txOutbd = findLazyTxOutBySingleToken txInfoOutputsBd ownSymbolBd (create100Tn newElement) -- ^ find output by 100 token
        in
           -- ^ exactly 3 tokens minted
           lengthListTupleBd thisPolicyMintTnAm 0 == 3
           -- ^ exactly 1 new index token minted
        && builtinsElemMapBd thisPolicyMintTnAm indexTokenNameBd (BI.mkI 1)
           -- ^ exactly 1 global state 100 token minted
        && builtinsElemMapBd thisPolicyMintTnAm (create100Tn newElement) (BI.mkI 1)
           -- ^ exactly 1 global state 222 token minted
        && builtinsElemMapBd thisPolicyMintTnAm (create222Tn newElement) (BI.mkI 1)
           -- ^ there is an output now pointing to the new name
        && checkNewOutput (mkInlineDatumBuiltin $ toBuiltinData (PPr.fst dat, newElement))
           -- ^ there is an output pointing to the next element
        && checkNewOutput (mkInlineDatumBuiltin $ toBuiltinData (newElement, PPr.snd dat))
           -- ^ new name is between of input data and next
        && (PPr.fst dat < newElement && newElement < PPr.snd dat)
           -- ^ 100 token is send to global state address
        && lazyTxOutAddrBd txOutbd == stateAddr
           -- ^ 100 token has correct datum
        && lazyTxOutDatumBd txOutbd == mkInlineDatumBuiltin (toBuiltinData $ GlobalStateDatum (CurrencySymbol $ BI.unsafeDataAsB ownSymbolBd) newElement [])
           -- ^ 100 token has no reference script attached 
        && lazyTxOutReferenceScriptBd txOutbd == nothingBd
           -- ^ 100 token has exactly 2 currency symbols attached (ada + own symbol)
        && lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd txOutbd) 0 == 2

    where
        -- own input datum
        getOwnInputDatum :: BuiltinList BuiltinData -> (BuiltinByteString, BuiltinByteString)
        getOwnInputDatum txInInfosBd
          | B.null txInInfosBd = error ()
          | oneTokenInValue (lazyTxInInfoValueMapBd $ BI.head txInInfosBd) = unsafeFromBuiltinData $ lazyTxInInfoTxOutInlineDatBd (BI.head txInInfosBd)
          | otherwise = getOwnInputDatum (BI.tail txInInfosBd)

        -- either boarder token or own symbol
        oneTokenInValue :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Bool
        oneTokenInValue value = singleTokenInValueBd value ownSymbolBd indexTokenNameBd 
                             || singleTokenInValueBd value startEndCs indexTokenNameBd

        -- new output present
        checkNewOutput :: BuiltinData -> Bool
        checkNewOutput outDat = go txInfoOutputsBd
          where
            go :: BuiltinList BuiltinData -> Bool
            go bdList
              | B.null bdList = error ()
              | lazyTxOutAddrBd (BI.head bdList) == addressFromScriptHashNoStakeBd ownSymbolBd && lazyTxOutDatumBd (BI.head bdList) == outDat = lazyTxOutReferenceScriptBd (BI.head bdList) == nothingBd
                                                                                              && oneTokenInValue (lazyTxOutValueMapBd $ BI.head bdList)
                                                                                              && lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd $ BI.head bdList) 0 == 2
              | otherwise = go (BI.tail bdList) 