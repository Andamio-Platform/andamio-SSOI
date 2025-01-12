module Andamio.ProveCreds.Compiled
    ( proveCredsValidatorSerialised
    , ProveCredsDatum(..)
    ) where

import Prelude                                        ((.))
import PlutusLedgerApi.V3                             (serialiseCompiledCode, SerialisedScript, BuiltinData)

import Andamio.ProveCreds.OnChain.ProveCredsValidator (proveCredsValidatorCompiledCode, ProveCredsDatum(..))

proveCredsValidatorSerialised :: BuiltinData -> SerialisedScript
proveCredsValidatorSerialised = serialiseCompiledCode . proveCredsValidatorCompiledCode