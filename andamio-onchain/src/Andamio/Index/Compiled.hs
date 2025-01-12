module Andamio.Index.Compiled
    ( -- IndexValidator
      indexScriptsSerialised
    , IndexParams(..)
      -- InitIndexPolicy
    , initIndexPolicySerialised
      -- Utils
    , indexTokenName
    ) where

import Prelude                               ((.))
import PlutusLedgerApi.V3                    (BuiltinData, serialiseCompiledCode,
                                             SerialisedScript, unsafeFromBuiltinData,
                                             TokenName)
import Andamio.Index.OnChain.IndexScripts    (IndexParams(..), indexScriptsCompiledCode)
import Andamio.Index.OnChain.InitIndexPolicy (initIndexPolicyCompiledCode, indexTokenNameBd)

indexTokenName :: TokenName
indexTokenName = unsafeFromBuiltinData indexTokenNameBd

indexScriptsSerialised :: IndexParams -> SerialisedScript
indexScriptsSerialised = serialiseCompiledCode . indexScriptsCompiledCode

initIndexPolicySerialised :: BuiltinData -> SerialisedScript
initIndexPolicySerialised = serialiseCompiledCode . initIndexPolicyCompiledCode