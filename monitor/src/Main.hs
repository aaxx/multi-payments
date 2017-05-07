
module Main where

import           Control.Monad (void)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Configurator as Config

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified System.Environment as Env
import qualified System.Posix.Signals as Signal

import BlockchainInfo
import Util


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




