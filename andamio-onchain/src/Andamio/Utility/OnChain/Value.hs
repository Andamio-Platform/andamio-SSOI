
{-# LANGUAGE DerivingStrategies #-}


{-# LANGUAGE MultiParamTypeClasses #-}


module Andamio.Utility.OnChain.Value
    ( csElemList
    , valueOfBd
    , singleTokenInValueBd
    , create222Tn
    , create100Tn
    , lengthListTupleBd
    , builtinsElemMapBd
    , tnAmBdMapByCsFromValueBd
    , checkMinting
    , csElem
    , lengthBuiltinValueIf1Tn
    ) where

import PlutusLedgerApi.V3               (CurrencySymbol (..))
import PlutusTx.Prelude                 (Eq(..), Integer, BuiltinByteString,
                                        (&&), ($), (+), Bool(..), (||),
                                        error, BuiltinData, otherwise)
import PlutusTx.Builtins.Internal as BI (head, tail, BuiltinList(..), fst, mkB, mkI,
                                        BuiltinPair, snd, unsafeDataAsMap, unsafeDataAsI,
                                        appendByteString, BuiltinInteger)
import PlutusTx.Builtins          as B  (null)

-- ^ "a" -> "222a"
{-# INLINEABLE create222Tn #-}
create222Tn :: BuiltinByteString -> BuiltinData
create222Tn alias = BI.mkB $ appendByteString "222" alias

-- ^ "a" -> "100a"
{-# INLINEABLE create100Tn #-}
create100Tn :: BuiltinByteString -> BuiltinData
create100Tn alias = BI.mkB $ appendByteString "100" alias

-- ^ validate if CurrencySymbol is in list
{-# INLINEABLE csElemList #-}
csElemList :: [CurrencySymbol] -> CurrencySymbol -> Bool
csElemList [] _ = False
csElemList (x:xs) cs = (x == cs) || csElemList xs cs

-- ^ validate 1 token with name and specified amount is in value
-- ^ Map TokenName Integer -> TokenName -> Integer -> Bool
{-# INLINEABLE checkMinting #-}
checkMinting :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinByteString -> BuiltinInteger -> Bool
checkMinting bdTupleList userName am = lengthListTupleBd bdTupleList 0 == 1 &&
                                       BI.fst (BI.head bdTupleList) == BI.mkB userName &&
                                       BI.snd (BI.head bdTupleList) == BI.mkI am

-- ^ get token name amount map from currency symbol token name integer map
-- ^ Map CurrencySymbol (Map TokenName Integer) -> CurrencySymbol -> Map TokenName Integer
{-# INLINEABLE tnAmBdMapByCsFromValueBd #-}
tnAmBdMapByCsFromValueBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData)
tnAmBdMapByCsFromValueBd bdTupleList csBd
  | B.null bdTupleList = error ()
  | BI.fst (BI.head bdTupleList) == csBd = BI.unsafeDataAsMap (BI.snd $ BI.head bdTupleList)
  | otherwise = tnAmBdMapByCsFromValueBd (BI.tail bdTupleList) csBd

-- ^ [(a, b)] -> a -> b -> Bool
{-# INLINEABLE builtinsElemMapBd #-}
builtinsElemMapBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> BuiltinData -> Bool
builtinsElemMapBd bdTupleList fstBd sndBd
  | B.null bdTupleList = error ()
  | BI.fst (BI.head bdTupleList) == fstBd = BI.snd (BI.head bdTupleList) == sndBd
  | otherwise = builtinsElemMapBd (BI.tail bdTupleList) fstBd sndBd

-- ^ count length of BuiltinData list
{-# INLINEABLE lengthListTupleBd #-}
lengthListTupleBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Integer -> Integer
lengthListTupleBd bdTupleList !counter
  | B.null bdTupleList = counter
  | otherwise = lengthListTupleBd (BI.tail bdTupleList) (counter + 1)

-- ^ count length of BuiltinData list
{-# INLINEABLE lengthBuiltinValueIf1Tn #-}
lengthBuiltinValueIf1Tn :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Integer -> Integer
lengthBuiltinValueIf1Tn bdTupleList !counter
  | B.null bdTupleList = counter
  | lengthListTupleBd (BI.unsafeDataAsMap $ BI.snd $ BI.head bdTupleList) 0 == 1 = lengthBuiltinValueIf1Tn (BI.tail bdTupleList) (counter + 1)
  | otherwise = error ()

-- ^ BuiltinData is present as first element of tuple list
-- ^ [(a, b)] -> a -> Bool
{-# INLINEABLE csElem #-}
csElem :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> Bool
csElem value cs
      | B.null value = False
      | BI.fst (BI.head value) == cs = True
      | otherwise = csElem (BI.tail value) cs

-- ^ valueOf but with BuiltinData
-- ^ Map CurrencySymbol (Map TokenName Integer) -> CurrencySymbol -> TokenName -> Integer
{-# INLINEABLE valueOfBd #-}
valueOfBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> BuiltinData -> Integer
valueOfBd value cs tn = go1 value
  where
    go1 :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Integer
    go1 bdTupleList
      | B.null bdTupleList = 0
      | BI.fst (BI.head bdTupleList) == cs = go2 (BI.unsafeDataAsMap $ BI.snd $ BI.head bdTupleList)
      | otherwise = go1 (BI.tail bdTupleList)

    go2 :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Integer
    go2 bdTupleList
      | B.null bdTupleList = 0
      | BI.fst (BI.head bdTupleList) == tn = BI.unsafeDataAsI (BI.snd $ BI.head bdTupleList)
      | otherwise = go2 (BI.tail bdTupleList)

-- ^ amount of a token in value is exactly one
-- ^ Map CurrencySymbol (Map TokenName Integer) -> CurrencySymbol -> TokenName -> Bool
{-# INLINEABLE singleTokenInValueBd #-}
singleTokenInValueBd :: BuiltinList (BuiltinPair BuiltinData BuiltinData) -> BuiltinData -> BuiltinData -> Bool
singleTokenInValueBd value cs tn = valueOfBd value cs tn == 1