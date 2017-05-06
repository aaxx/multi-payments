
module Main where

import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO, threadDelay)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Configurator as Config
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           System.IO (stderr)
import qualified System.Environment as Env

import qualified Network.WebSockets as WS
import qualified Wuss


main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      conf <- Config.load [Config.Required configPath]

      logInfo $ "Reading config from " <> T.pack configPath

      pgHost    <- Config.require conf "pg.host"
      pgPort    <- Config.require conf "pg.port"
      pgUser    <- Config.require conf "pg.user"
      pgPwd     <- Config.require conf "pg.pass"
      pgDb      <- Config.require conf "pg.db"

      logInfo $ "Connecting to blockchain.info"
      blockchainInfo

      logInfo $ "Connecting to Postgres on " <> T.pack pgHost
      let cInfo = PG.ConnectInfo
            pgHost pgPort
            pgUser pgPwd
            pgDb
      pgPool <- createPool (PG.connect cInfo) PG.close
          1 -- number of distinct sub-pools
            -- time for which an unused resource is kept open
          (fromInteger 20) -- seconds
          5 -- maximum number of resources to keep open

      return ()

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


blockchainInfo :: IO ()
blockchainInfo = do
  let bcInfo = "ws.blockchain.info"
  Wuss.runSecureClient bcInfo 443 "/inv" $ \conn -> do
    logInfo "Connected"

    void . forkIO . forever $ do
      WS.receiveData conn >>= T.putStrLn

    WS.sendTextData conn $ Aeson.encode $ Aeson.object
          [ "op" .= txt "unconfirmed_sub" ]
    forever $ threadDelay 10000


-----------
-- Utility
-----------


logInfo :: Text -> IO ()
logInfo = T.hPutStrLn stderr

txt :: Text -> Text
txt = id
