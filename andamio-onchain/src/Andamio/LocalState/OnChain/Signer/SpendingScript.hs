module Andamio.LocalState.OnChain.Signer.SpendingScript
    ( validateSpending
    ) where

import PlutusTx                                             (toBuiltinData)
import PlutusTx.Prelude                                     (BuiltinData, Bool(..), (==), (&&))
import PlutusTx.Builtins.Internal                     as BI (BuiltinList(..), BuiltinByteString, mkB)

import Andamio.LocalState.OnChain.Signer.SignerParams       (SignerParams(..))
import Andamio.Utility.OnChain.TxInInfo                   (findLazyTxInInfoByTxOutRefToTxOutBd)
import Andamio.Utility.OnChain.Value                      (singleTokenInValueBd, lengthBuiltinValueIf1Tn)
import Andamio.Utility.OnChain.Datum                      (mkInlineDatumBuiltin, nothingBd)
import Andamio.Utility.OnChain.BuiltinByteString          (bdElemList)
import Andamio.Utility.OnChain.Address                    (getAddressBdScriptHashBd)
import Andamio.Utility.OnChain.TxOut                      (lazyTxOutValueMapBd, lazyTxOutAddrBd,
                                                          lazyTxOutReferenceScriptBd,
                                                          findLazyTxOutBySingleToken,
                                                          lazyTxOutDatumBd, lazyTxOutAddrBd)

-- ^ Allows a to add something to local state if correct signature is present.

{-# INLINEABLE validateSpending #-}
validateSpending :: SignerParams -> (BuiltinByteString, BuiltinByteString) -> [BuiltinByteString] -> BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> Bool
validateSpending SignerParams{..} (userName, cred) dat ownInputTxOutRefBd txInfoInputsBd txInfoOutputsBd txInfoSignatoriesBd =

    let ownInputBd = findLazyTxInInfoByTxOutRefToTxOutBd txInfoInputsBd ownInputTxOutRefBd -- ^ find own input by tx out ref
        ownAddress = lazyTxOutAddrBd ownInputBd -- ^ this policy address
        ownSymbolBd = getAddressBdScriptHashBd ownAddress -- ^ own currency symbol
        ownTxOutBd = findLazyTxOutBySingleToken txInfoOutputsBd ownSymbolBd (BI.mkB userName) -- ^ minted token output tx out
    in
        -- ^ signer is present
        bdElemList txInfoSignatoriesBd spSignerPkh &&
        -- ^ own input has correct token
        singleTokenInValueBd (lazyTxOutValueMapBd ownInputBd) ownSymbolBd (BI.mkB userName) &&
        -- ^ output address correct
        lazyTxOutAddrBd ownTxOutBd == ownAddress &&
        -- ^ output datum is an empty list
        lazyTxOutDatumBd ownTxOutBd == mkInlineDatumBuiltin (toBuiltinData (cred:dat)) &&
        -- ^ no reference script in output
        lazyTxOutReferenceScriptBd ownTxOutBd == nothingBd &&
        -- ^ ada + token as output
        lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd ownTxOutBd) 0 == 2