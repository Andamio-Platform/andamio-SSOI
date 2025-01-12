module Andamio.ProveCreds.OnChain.ProveCredsValidator
                    ( proveCredsValidatorCompiledCode
                    , ProveCredsDatum(..)
                    ) where

import qualified Prelude                                    as Pr (error, Either(..))
import           PlutusCore.Version                               (plcVersion110)
import           PlutusLedgerApi.V3                               (CurrencySymbol(..), BuiltinData, unsafeFromBuiltinData)
import           PlutusTx                                         (CompiledCode, compile, applyCode, liftCode)
import           PlutusTx.Prelude                                 ((&&), BuiltinUnit, Bool(..), error, Integer, (>=),
                                                                  BuiltinByteString, otherwise, ($), fst, snd) 
import           PlutusTx.Builtins.Internal                 as BI (BuiltinList, mkB, head, tail, unitval)
import           PlutusTx.Builtins                          as B  (null)
import           Andamio.ProveCreds.OnChain.ProveCredsDatum       (ProveCredsDatum(..))
import           Andamio.Utility.OnChain.TxInInfo                 (singleTokenInOutAddrSame, lazyTxInInfoTxOutInlineDatBd,
                                                                  lazyTxInInfoTxOutInlineDatBd, lazyTxInInfoValueMapBd)
import           Andamio.Utility.OnChain.Value                    (singleTokenInValueBd, create222Tn)
import           Andamio.Utility.OnChain.BuiltinByteString        (allElem)
import           Andamio.Utility.OnChain.LazyContextV3            (lazyTxInfoOutputsBd, lazyTxInfoInputsBd,
                                                                  lazyRedeemerTyped, lazyTxInfoReferenceInputsBd,
                                                                  lazyInlineDatumTyped)

-- ^ Simple spending validator to prove someone has credentials. 
-- ^ Tokens are locked and if the owner of a 222 token can provides all credentials defined in the datum as 
-- ^ reference inputs he can unlock the tokens.

{-# INLINEABLE validateSpending #-}
validateSpending :: BuiltinData -> BuiltinByteString -> ProveCredsDatum -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> BuiltinList BuiltinData -> Bool
validateSpending globalCsBd userName dat txInfoInputsBd txInfoOutputsBd txInfoReferenceInputsBd =

                -- ^ check that user has all builtin byte string credentials
                bbsCredPresent txInfoReferenceInputsBd (getBbsCreds dat)
                -- ^ check that user has greater or equal integer credential
            &&  intCredPresent txInfoReferenceInputsBd (getIntCreds dat)
                -- ^ check 222 token is present and input/output is same
            &&  singleTokenInOutAddrSame txInfoInputsBd txInfoOutputsBd globalCsBd (create222Tn userName)
  where
    -- ^ iterates through reference inputs and checks if all credentials are present in datum
    bbsCredPresent :: BuiltinList BuiltinData -> [(CurrencySymbol, [BuiltinByteString])] -> Bool
    bbsCredPresent _ [] = True
    bbsCredPresent refInputsBd (x:xs)
      | B.null refInputsBd = error ()
      | singleTokenInValueBd (lazyTxInInfoValueMapBd $ BI.head refInputsBd) (BI.mkB $ unCurrencySymbol $ fst x) (BI.mkB userName) = if allElem (snd x) (unsafeFromBuiltinData @[BuiltinByteString] (lazyTxInInfoTxOutInlineDatBd $ BI.head refInputsBd)) then bbsCredPresent txInfoReferenceInputsBd xs else error ()
      | otherwise = bbsCredPresent (BI.tail refInputsBd) (x:xs)
    
    -- ^ iterates through reference inputs and checks if integer is equal or greater in datum
    intCredPresent :: BuiltinList BuiltinData -> [(CurrencySymbol, Integer)] -> Bool
    intCredPresent _ [] = True
    intCredPresent refInputsBd (x:xs)
      | B.null refInputsBd = error ()
      | singleTokenInValueBd (lazyTxInInfoValueMapBd $ BI.head refInputsBd) (BI.mkB $ unCurrencySymbol $ fst x) (BI.mkB userName) = if snd x >= unsafeFromBuiltinData @Integer (lazyTxInInfoTxOutInlineDatBd $ BI.head refInputsBd) then intCredPresent txInfoReferenceInputsBd xs else error ()
      | otherwise = intCredPresent (BI.tail refInputsBd) (x:xs)


{-# INLINEABLE untypedValidator #-}
untypedValidator ::BuiltinData -> BuiltinData -> BuiltinUnit
untypedValidator bd ctx'
  | validateSpending bd (lazyRedeemerTyped ctx') (lazyInlineDatumTyped ctx') (lazyTxInfoInputsBd ctx') (lazyTxInfoOutputsBd ctx') (lazyTxInfoReferenceInputsBd ctx') = unitval
  | otherwise = error ()

proveCredsValidatorCompiledCode :: BuiltinData -> CompiledCode (BuiltinData -> BuiltinUnit)
proveCredsValidatorCompiledCode bd = case compiled of 
    Pr.Right c -> c
    Pr.Left _ -> Pr.error "proveCredsValidatorCompiledCode"
    where
        compiled = $$(PlutusTx.compile [||untypedValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion110 bd