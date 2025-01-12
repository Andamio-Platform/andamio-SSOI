module Main (main) where

import           Control.Exception           (try)
import           Control.Monad.Trans.Except
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Environment          (getArgs)

import           GeniusYield.GYConfig

import           Andamio.API.API
import           Andamio.API.Context
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy.Char8  as BL8
import           Prelude                     hiding (read)

-- | Getting paths for our core and Andamio configurations.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
  args <- getArgs
  case args of
    ["--core-config", coreCfg, "--andamio-config", andamioCfg] -> return (coreCfg, andamioCfg)
    _ -> fail "Error: wrong arguments, expected --core-config <path> --andamio-config <path>\n"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  putStrLn "Parsing Configurations ..."
  (coreCfgPath, andamioCfgPath) <- parseArgs
  coreCfg <- coreConfigIO coreCfgPath
  andamioCfg <- readAndamioConfig andamioCfgPath

  putStrLn "Core Configuration ..."
  print coreCfg

  putStrLn "Andamio Configuration ..."
  print andamioCfg

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server"  $ \providers -> do
    let port = 8081
        ctx = Ctx coreCfg providers
    putStrLn $ "Starting server at \nhttp://localhost:" <> show port
    run port $ app ctx andamioCfg

app :: Ctx -> AndamioConfig -> Application
app ctx cfg = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $ serve andamioAPI $ hoistServer andamioAPI (Handler . ExceptT . try)  $ apiServer ctx cfg
