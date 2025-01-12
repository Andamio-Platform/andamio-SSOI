module Andamio.LocalState.OnChain.Signer.MintingScript
    ( validateMinting
    ) where

import PlutusTx.Prelude                                     (BuiltinData, Bool(..), (==), (&&))
import PlutusTx.Builtins.Internal                     as BI (BuiltinList(..), BuiltinByteString,
                                                            mkB, BuiltinPair, unsafeDataAsMap)
import PlutusLedgerApi.V3                                   (toBuiltinData)

import Andamio.LocalState.OnChain.Signer.SignerParams       (SignerParams(..))
import Andamio.LocalState.OnChain.Utility                   (checkGlobalRedPresent)
import Andamio.Utility.OnChain.TxInInfo                     (findLazyTxInInfoByToken,
                                                            lazyTxInInfoTxOutRefBd)
import Andamio.Utility.OnChain.Value                        (create100Tn, lengthBuiltinValueIf1Tn,
                                                            checkMinting, tnAmBdMapByCsFromValueBd)
import Andamio.Utility.OnChain.Datum                        (mkInlineDatumBuiltin, nothingBd)
import Andamio.Utility.OnChain.Address                      (addressFromScriptHashNoStakeBd)
import Andamio.Utility.OnChain.TxOut                        (lazyTxOutValueMapBd, lazyTxOutAddrBd,
                                                            lazyTxOutReferenceScriptBd,
                                                            findLazyTxOutBySingleToken,
                                                            lazyTxOutDatumBd, lazyTxOutAddrBd)

-- ^ Allows a global state token owner to mint this local state token.

{-# INLINEABLE validateMinting #-}
validateMinting :: SignerParams -> BuiltinByteString -> BuiltinData -> BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Bool
validateMinting SignerParams{..} userName ownSymbolBd txInfoMintBd txInfoInputsBd txInfoOutputsBd txInfoRedeemersBd =

    let thisPolicyMintTnAm = tnAmBdMapByCsFromValueBd (BI.unsafeDataAsMap txInfoMintBd) ownSymbolBd -- ^ this currency symbol minted
        ownAddress = addressFromScriptHashNoStakeBd ownSymbolBd -- ^ this policy address
        ownTxOutBd = findLazyTxOutBySingleToken txInfoOutputsBd ownSymbolBd (BI.mkB userName) -- ^ minted token output tx out
        global100txInInfoBd = lazyTxInInfoTxOutRefBd (findLazyTxInInfoByToken txInfoInputsBd spGlobalCs (create100Tn userName)) -- ^ find global state tx in info
    in
        -- ^ exactly 1 token minted
        checkMinting thisPolicyMintTnAm userName 1 &&
        -- ^ output address correct
        lazyTxOutAddrBd ownTxOutBd == ownAddress &&
        -- ^ output datum is an empty list
        lazyTxOutDatumBd ownTxOutBd == mkInlineDatumBuiltin (toBuiltinData ([] :: [BuiltinByteString])) &&
        -- ^ no reference script in output
        lazyTxOutReferenceScriptBd ownTxOutBd == nothingBd &&
        -- ^ ada + token as output
        lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd ownTxOutBd) 0 == 2 &&
        -- ^ check if global state redeemer with own currency symbol is present
        checkGlobalRedPresent txInfoRedeemersBd global100txInInfoBd ownSymbolBd