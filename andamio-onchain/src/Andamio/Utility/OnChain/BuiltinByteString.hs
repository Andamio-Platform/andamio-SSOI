module Andamio.Utility.OnChain.BuiltinByteString
    ( bdElemList
    , allElem
    ) where

import PlutusTx.Prelude                      (Bool(..), (==), otherwise, (||), (&&))
import PlutusLedgerApi.V3                    (BuiltinByteString, BuiltinData)
import PlutusTx.Builtins.Internal            (head, BuiltinList, tail)
import PlutusTx.Builtins                     (null)

-- ^ BuiltinData is in BuiltinData List
-- ^ BuiltinData `elem` [BuiltinData]
{-# INLINEABLE bdElemList #-}
bdElemList :: BuiltinList BuiltinData -> BuiltinData -> Bool
bdElemList bdList bd
  | null bdList = False
  | otherwise = (head bdList == bd) || bdElemList (tail bdList) bd

-- ^ first BuiltinByteString list exists in second
-- ^ all (`elem` [BuiltinByteString]) [BuiltinByteString]
{-# INLINEABLE allElem #-}
allElem :: [BuiltinByteString] -> [BuiltinByteString] -> Bool
allElem [] _ = True
allElem (x:xs) userLs = bbsIsElem userLs x && allElem xs userLs

-- BuiltinByteString is in BuiltinByteString list
-- ^ BuiltinByteString `elem` [BuiltinByteString]
{-# INLINEABLE bbsIsElem #-}
bbsIsElem :: [BuiltinByteString] -> BuiltinByteString -> Bool
bbsIsElem [] _ = False
bbsIsElem (x:xs) bbs = (x == bbs) || bbsIsElem xs bbs