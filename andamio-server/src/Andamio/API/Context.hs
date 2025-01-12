module Andamio.API.Context
    ( AndamioConfig(..)
    , Ctx(..)
    , runQuery
    , runTxI
    , runTxF
    , readAndamioConfig
    ) where

import           GHC.Generics            (Generic)
import           Data.Aeson              (eitherDecodeFileStrict, FromJSON)
import           GeniusYield.GYConfig    (GYCoreConfig(..))
import           GeniusYield.Imports     (Identity(..), throwIO, coerce)
import           GeniusYield.Transaction (GYCoinSelectionStrategy(..))
import           GeniusYield.TxBuilder   (GYTxSkeleton, GYTxQueryMonadIO, 
                                         GYTxBuilderMonadIO, runGYTxQueryMonadIO,
                                         runGYTxBuilderMonadIO, buildTxBodyWithStrategy)
import           GeniusYield.Types       (GYAddressBech32, GYTxOutRef, GYProviders, 
                                         GYAddress, GYTxOutRefCbor(..), GYTxBody,
                                         GYNetworkId)
import           Prelude                 (Either(..), IO, FilePath, (>>=),
                                         Maybe(..), Show, Traversable, traverse,
                                         Bool(..), return, (<>), ($), userError)


data AndamioConfig = AndamioConfig
    {   seedTxRef        :: !GYTxOutRef
    ,   referenceAddress :: !GYAddressBech32
    } deriving (Show, Generic, FromJSON)

data Ctx = Ctx
    {   ctxCoreCfg   :: !GYCoreConfig
    ,   ctxProviders :: !GYProviders
    }

runQuery :: Ctx -> GYTxQueryMonadIO a -> IO a
runQuery ctx q = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers q

-- | Wraps our skeleton under `Identity` and calls `runTxF`.
runTxI :: Ctx
       -> [GYAddress]           -- ^ User's used addresses.
       -> GYAddress             -- ^ User's change address.
       -> Maybe GYTxOutRefCbor  -- ^ Browser wallet's reserved collateral (if set).
       -> GYTxBuilderMonadIO (GYTxSkeleton v)
       -> IO GYTxBody
runTxI = coerce (runTxF @Identity)

runGYTxMonadNodeF :: forall t v. Traversable t => GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (t (GYTxSkeleton v)) -> IO (t GYTxBody)
runGYTxMonadNodeF strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= traverse (buildTxBodyWithStrategy strat)

-- | Tries to build for given skeletons wrapped under traversable structure.
runTxF :: Traversable t
       => Ctx
       -> [GYAddress]           -- ^ User's used addresses.
       -> GYAddress             -- ^ User's change address.
       -> Maybe GYTxOutRefCbor  -- ^ Browser wallet's reserved collateral (if set).
       -> GYTxBuilderMonadIO (t (GYTxSkeleton v))
       -> IO (t GYTxBody)
runTxF ctx addrs addr collateral skeleton  = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxMonadNodeF GYRandomImproveMultiAsset nid providers addrs addr
    (collateral >>=
      (\c -> Just (getTxOutRefHex c,
                   False  -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                  )
      )
    ) skeleton


-- | Load AndamioConfig from a JSON file.
readAndamioConfig :: FilePath -> IO AndamioConfig
readAndamioConfig path = do
  result <- eitherDecodeFileStrict path
  case result of
    Left err  -> throwIO $ userError $ "Error parsing AndamioConfig: " <> err
    Right cfg -> return cfg
