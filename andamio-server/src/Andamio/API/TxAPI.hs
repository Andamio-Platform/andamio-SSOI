module Andamio.API.TxAPI
      ( TxAPI
      , handleTxV0API) where

import           Andamio.API.Context                 (Ctx, AndamioConfig)
import           Prelude                             (IO)
import           Servant                 
import           Andamio.API.Tx.MintSSOI             (MintSSOITxRequest, handleMintSSOI)
import           Andamio.API.Tx.InitSSOISystem       (InitSSOISystemTxRequest, handleInitSSOISystem)
import           Andamio.API.Tx.InitCredentialSigner (InitCredentialSignerTxRequest, handleInitCredentialSigner)
import           Andamio.API.Tx.MintLocalStateSigner (MintLocalStateSignerTxRequest, handleMintLocalStateSigner)
import           Andamio.API.Tx.AddCredentialSigner  (AddCredentialSignerTxRequest, handleAddCredentialSigner)
import           Andamio.API.Tx.InitCredentialToken  (InitCredentialTokenTxRequest, handleInitCredentialToken)
import           Andamio.API.Tx.MintLocalStateToken  (MintLocalStateTokenTxRequest, handleMintLocalStateToken)
import           Andamio.API.Tx.AddCredentialToken   (AddCredentialTokenTxRequest, handleAddCredentialToken)
import           Andamio.API.Tx.ProveCredentials     (ProveCredentialsTxRequest, handleProveCredentials)
import           Andamio.API.Tx.AddRewards           (AddRewardsTxRequest, handleAddRewards)
import           Andamio.API.Tx.UnsignedTxResponse   (UnsignedTxResponse (..))

type TxAPI =
          Summary "Handles the request to initialise the ssoi system."
        :>  Description "This endpoint allows to submit transaction information for initialising the ssoi system."
        :>  "init-ssoi-system"
        :>  ReqBody '[JSON] InitSSOISystemTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to mint a new ssoi token."
        :>  Description "This endpoint allows everyone to submit transaction information for minting a new ssoi token."
        :>  "mint-ssoi"
        :>  ReqBody '[JSON] MintSSOITxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to initialise a credential issuer."
        :>  Description "This endpoint allows everyone to submit transaction information for initialising a credential issuer restricted by a public key hash."
        :>  "init-credential-signer"
        :>  ReqBody '[JSON] InitCredentialSignerTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to initialise a credential issuer."
        :>  Description "This endpoint allows everyone to submit transaction information for initialising a credential issuer restricted by an alias."
        :>  "init-credential-token"
        :>  ReqBody '[JSON] InitCredentialTokenTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to add credentials with correct public key hash."
        :>  Description "This endpoint allows everyone to submit transaction information for adding credentials to a specific utxo."
        :>  "add-credential-signer"
        :>  ReqBody '[JSON] AddCredentialSignerTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to add credentials with correct token alias."
        :>  Description "This endpoint allows everyone to submit transaction information for adding credentials to a specific utxo."
        :>  "add-credential-token"
        :>  ReqBody '[JSON] AddCredentialTokenTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to mint a local state signer."
        :>  Description "This endpoint allows holder of a global state token to mint a local state signer."
        :>  "mint-local-state-signer"
        :>  ReqBody '[JSON] MintLocalStateSignerTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to mint a local state token."
        :>  Description "This endpoint allows holder of a global state token to mint a local state token."
        :>  "mint-local-state-token"
        :>  ReqBody '[JSON] MintLocalStateTokenTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to unlock a reward if credentials are provided."
        :>  Description "This endpoint allows users with correct credentials to unlock a reward."
        :>  "prove-credentials"
        :>  ReqBody '[JSON] ProveCredentialsTxRequest
        :>  Post    '[JSON] UnsignedTxResponse
     :<|> Summary "Handles the request to add a reward if credentials are provided."
        :>  Description "This endpoint allows users to add a reward."
        :>  "add-rewards"
        :>  ReqBody '[JSON] AddRewardsTxRequest
        :>  Post    '[JSON] UnsignedTxResponse

handleTxV0API :: Ctx -> AndamioConfig -> ServerT TxAPI IO
handleTxV0API ctx andamioConf = handleInitSSOISystem ctx andamioConf
                           :<|> handleMintSSOI ctx andamioConf
                           :<|> handleInitCredentialSigner ctx andamioConf
                           :<|> handleInitCredentialToken ctx andamioConf
                           :<|> handleAddCredentialSigner ctx andamioConf
                           :<|> handleAddCredentialToken ctx andamioConf
                           :<|> handleMintLocalStateSigner ctx andamioConf
                           :<|> handleMintLocalStateToken ctx andamioConf
                           :<|> handleProveCredentials ctx andamioConf
                           :<|> handleAddRewards ctx andamioConf