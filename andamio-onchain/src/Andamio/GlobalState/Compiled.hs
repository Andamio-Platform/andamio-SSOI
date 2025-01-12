module Andamio.GlobalState.Compiled
    ( globalStateValidatorSerialised
    , GlobalStateDatum(..)
    ) where

import PlutusLedgerApi.V3                               (SerialisedScript, serialiseCompiledCode)
import Andamio.GlobalState.OnChain.GlobalStateValidator (globalStateValidatorCompiledCode)
import Andamio.GlobalState.OnChain.GlobalStateDatum     (GlobalStateDatum(..))

globalStateValidatorSerialised :: SerialisedScript
globalStateValidatorSerialised = serialiseCompiledCode globalStateValidatorCompiledCode