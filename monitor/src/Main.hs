
module Main where

import           Control.Monad (forever, void)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Configurator as Config
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           System.IO (stderr)
import qualified System.Environment as Env
import qualified System.Posix.Signals as Signal

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

      -- handle interrupts gracefully
      interruptFlag <- newEmptyMVar :: IO (MVar ())
      let handleSig = Signal.Catch $ putMVar interruptFlag ()
      void $ Signal.installHandler Signal.sigINT handleSig Nothing
      void $ Signal.installHandler Signal.sigTERM handleSig Nothing


      logInfo $ "Connecting to Postgresql on " <> T.pack pgHost
      let cInfo = PG.ConnectInfo
            pgHost pgPort
            pgUser pgPwd
            pgDb
      pgPool <- createPool (PG.connect cInfo) PG.close
          1 -- number of distinct sub-pools
            -- time for which an unused resource is kept open
          (fromInteger 20) -- seconds
          5 -- maximum number of resources to keep open

      logInfo $ "Connecting to blockchain.info"
      blockchainInfo interruptFlag

      readMVar interruptFlag
      logInfo "Terminating due to signal\n"
      sleep 3 -- give some time to release resources

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


blockchainInfo :: MVar () -> IO ()
blockchainInfo intFlag = do
  let bcInfo = "ws.blockchain.info"
  Wuss.runSecureClient bcInfo 443 "/inv" $ \conn -> do
    void . forkIO . forever
      $ WS.receiveData conn >>= T.putStrLn

    void . forkIO . forever $ do
      sleep 30
      WS.sendPing conn $ json ["op" .= txt "ping"]

    WS.sendTextData conn $ json ["op" .= txt "unconfirmed_sub"]

    readMVar intFlag
    WS.sendClose conn $ txt "See ya!"


-----------
-- Utility
-----------

sleep :: Int -> IO ()
sleep seconds = threadDelay $ seconds * 10^(6 :: Int)

logInfo :: Text -> IO ()
logInfo = T.hPutStrLn stderr

txt :: Text -> Text
txt = id

json :: [(Text, Aeson.Value)] -> LB.ByteString
json = Aeson.encode . Aeson.object
