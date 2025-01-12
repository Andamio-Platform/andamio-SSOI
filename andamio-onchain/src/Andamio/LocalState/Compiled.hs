module Andamio.LocalState.Compiled
    ( localStateSignerSerialised
    , SignerParams(..)
    , localStateTokenSerialised
    , TokenParams(..)
    ) where

import Prelude                                  ((.))
import PlutusLedgerApi.V3                       (serialiseCompiledCode, SerialisedScript)

import Andamio.LocalState.OnChain.SignerScripts (localStateSignerCompiledCode, SignerParams(..))
import Andamio.LocalState.OnChain.TokenScripts  (localStateTokenCompiledCode, TokenParams(..))

localStateSignerSerialised :: SignerParams -> SerialisedScript
localStateSignerSerialised = serialiseCompiledCode . localStateSignerCompiledCode

localStateTokenSerialised :: TokenParams -> SerialisedScript
localStateTokenSerialised = serialiseCompiledCode . localStateTokenCompiledCode