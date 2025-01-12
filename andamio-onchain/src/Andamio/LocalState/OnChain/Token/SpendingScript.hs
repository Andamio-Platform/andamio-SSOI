module Andamio.LocalState.OnChain.Token.SpendingScript
    ( validateSpending
    ) where

import PlutusTx.Prelude                                   (BuiltinData, Bool(..), (==), (&&), Integer, (+))
import PlutusTx.Builtins.Internal                   as BI (BuiltinList(..), BuiltinByteString, mkB, mkI)

import Andamio.LocalState.OnChain.Token.TokenParams       (TokenParams(..))
import Andamio.Utility.OnChain.TxInInfo                   (findLazyTxInInfoByTxOutRefToTxOutBd,
                                                          singleTokenInOutAddrSame)
import Andamio.Utility.OnChain.Value                      (singleTokenInValueBd, lengthBuiltinValueIf1Tn, create222Tn)
import Andamio.Utility.OnChain.Datum                      (mkInlineDatumBuiltin, nothingBd)
import Andamio.Utility.OnChain.Address                    (getAddressBdScriptHashBd)
import Andamio.Utility.OnChain.TxOut                      (lazyTxOutValueMapBd, lazyTxOutAddrBd,
                                                          lazyTxOutReferenceScriptBd,
                                                          findLazyTxOutBySingleToken,
                                                          lazyTxOutDatumBd, lazyTxOutAddrBd)

-- ^ Allows a to add something to local state if correct token is present.

{-# INLINEABLE validateSpending #-}
validateSpending :: TokenParams -> BuiltinByteString -> Integer -> BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> Bool
validateSpending TokenParams{..} userName dat ownInputTxOutRefBd txInfoInputsBd txInfoOutputsBd =

    let ownInputBd = findLazyTxInInfoByTxOutRefToTxOutBd txInfoInputsBd ownInputTxOutRefBd -- ^ find own input by tx out ref
        ownAddress = lazyTxOutAddrBd ownInputBd -- ^ this policy address
        ownSymbolBd = getAddressBdScriptHashBd ownAddress -- ^ own currency symbol
        ownTxOutBd = findLazyTxOutBySingleToken txInfoOutputsBd ownSymbolBd (BI.mkB userName) -- ^ minted token output tx out
    in
        -- ^ check if global state 222 token is present
        singleTokenInOutAddrSame txInfoInputsBd txInfoOutputsBd tpGlobalCs (create222Tn tpGlobalAlias) &&
        -- ^ own input has correct token
        singleTokenInValueBd (lazyTxOutValueMapBd ownInputBd) ownSymbolBd (BI.mkB userName) &&
        -- ^ output address correct
        lazyTxOutAddrBd ownTxOutBd == ownAddress &&
        -- ^ output datum is an empty list
        lazyTxOutDatumBd ownTxOutBd == mkInlineDatumBuiltin (BI.mkI (dat + 1)) &&
        -- ^ no reference script in output
        lazyTxOutReferenceScriptBd ownTxOutBd == nothingBd &&
        -- ^ ada + token as output
        lengthBuiltinValueIf1Tn (lazyTxOutValueMapBd ownTxOutBd) 0 == 2