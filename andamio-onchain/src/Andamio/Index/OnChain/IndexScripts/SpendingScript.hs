module Andamio.Index.OnChain.IndexScripts.SpendingScript
                    ( validateSpending
                    ) where


import PlutusTx.Prelude                 (BuiltinData, Integer, Bool(..),
                                        (==), (+), otherwise)
import PlutusTx.Builtins.Internal as BI (head, tail, BuiltinList(..), BuiltinPair(..), 
                                        mkConstr, mkNilData, mkCons, fst, unitval)
import PlutusTx.Builtins          as B  (null)

-- ^ Spending possible if exactly 1 own minting script present
{-# INLINEABLE validateSpending #-}
validateSpending :: BuiltinData -> BuiltinList (BuiltinPair BuiltinData BuiltinData) -> Integer -> Bool
validateSpending ownBbs txInfoRedeemersBd !counter
      | B.null txInfoRedeemersBd = counter == 1
      | BI.fst (BI.head txInfoRedeemersBd) == BI.mkConstr 0 (BI.mkCons ownBbs (BI.mkNilData unitval)) = validateSpending ownBbs (BI.tail txInfoRedeemersBd) (counter + 1)
      | otherwise = validateSpending ownBbs (BI.tail txInfoRedeemersBd) counter