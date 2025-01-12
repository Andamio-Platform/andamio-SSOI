module Andamio.API.Tx.AddRewards
    ( handleAddRewards
    , AddRewardsTxRequest (..)
    ) where

import           GHC.Generics                      (Generic)
import           Data.Aeson                        (FromJSON)
import           GeniusYield.Types                 (addressFromBech32, mintingPolicyId, GYMintingPolicyId, 
                                                   valueSingleton, GYTxOutRef, GYAddressBech32, GYTxOut(..), 
                                                   GYAddress, datumFromPlutusData, GYAssetClass(..),
                                                   PlutusVersion(..), GYTokenName, GYTxOutUseInlineDatum(..), 
                                                   valueFromLovelace)
import           GeniusYield.TxBuilder             (scriptAddress, GYTxSkeleton, GYTxQueryMonad, 
                                                   mustHaveOutput)
import           Prelude                           (Maybe(..), IO, Show, pure, ($), Integer, String, 
                                                   map, mconcat, (<>), return)
import           Data.Swagger           as Swagger (ToSchema)
import           Andamio.API.Context               (AndamioConfig (..), Ctx (..), 
                                                   runTxI, runQuery)
import           Andamio.Utility.Tx                (globalStateValidator, initIndexPolicy, indexScript,
                                                   createProveCredsDatum, proveCredsScript)
import           Andamio.Utility.API               (convertBech32AddressList, createCollateral)
import           Andamio.API.Tx.UnsignedTxResponse    (UnsignedTxResponse (..), unSignedTx')

data AddRewardsTxRequest = AddRewardsTxRequest
    {   usedAddresses           ::  ![GYAddressBech32]     -- ^ used wallet addresses
    ,   changeAddress           ::  !GYAddressBech32       -- ^ change address
    ,   collateralTxRef         ::  !(Maybe GYTxOutRef)    -- ^ maybe collateral tx ref
    ,   requiredIntCredentials  ::  ![(GYMintingPolicyId, Integer)]
    ,   requiredStrCredentials  ::  ![(GYMintingPolicyId, [String])]
    ,   lovelaceRewards         ::  !Integer
    ,   tokensToDeposit         ::  ![(GYMintingPolicyId, GYTokenName, Integer)] -- ^ tokens to deposit
    } deriving (Show, Generic, FromJSON, Swagger.ToSchema)

handleAddRewards :: Ctx -> AndamioConfig -> AddRewardsTxRequest -> IO UnsignedTxResponse
handleAddRewards ctx AndamioConfig{..} AddRewardsTxRequest{..} = do

        globalStateAddr <- runQuery ctx $ scriptAddress globalStateValidator
        let initPol = initIndexPolicy seedTxRef
            indexScr = indexScript (mintingPolicyId initPol) globalStateAddr
            proveCredScr = proveCredsScript (mintingPolicyId indexScr)
        proveCredAddr <- runQuery ctx $ scriptAddress proveCredScr

        txBody <- runTxI ctx (convertBech32AddressList usedAddresses) (addressFromBech32 changeAddress) (createCollateral collateralTxRef)
                    $ mkRewardsSkeleton proveCredAddr 
        pure $ unSignedTx' txBody

        where

            mkRewardsSkeleton :: GYTxQueryMonad m => GYAddress -> m (GYTxSkeleton 'PlutusV3)
            mkRewardsSkeleton addr  = return $ mustHaveOutput GYTxOut
                                                { gyTxOutAddress = addr
                                                , gyTxOutValue = valueFromLovelace lovelaceRewards <> (mconcat $ map (\(p, t, i) -> valueSingleton (GYToken p t) i) tokensToDeposit)
                                                , gyTxOutDatum = Just (datumFromPlutusData $ createProveCredsDatum requiredStrCredentials requiredIntCredentials, GYTxOutUseInlineDatum)
                                                , gyTxOutRefS = Nothing
                                                }