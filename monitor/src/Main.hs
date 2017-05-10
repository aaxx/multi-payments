
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
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
main = getArgs $ \configPath -> do
  logInfo $ "Reading config from " <> T.pack configPath
  conf <- Config.load [Config.Required configPath]
  logInfo "Connect to Postgresql"
  cInfo <- PG.ConnectInfo
    <$> Config.require conf "pg.host"
    <*> Config.require conf "pg.port"
    <*> Config.require conf "pg.user"
    <*> Config.require conf "pg.pass"
    <*> Config.require conf "pg.db"

  pgPool <- createPool (PG.connect cInfo) PG.close
      1 -- number of distinct sub-pools
        -- time for which an unused resource is kept open
      (fromInteger 20) -- seconds
      5 -- maximum number of resources to keep open

  -- handle interrupts gracefully
  interruptFlag <- newEmptyMVar :: IO (MVar ())
  let handleSig = Signal.Catch $ putMVar interruptFlag ()
  void $ Signal.installHandler Signal.sigINT handleSig Nothing
  void $ Signal.installHandler Signal.sigTERM handleSig Nothing

  -- FIXME: forever reconnect
  logInfo "Get blocks from blockchain.info"
  subscribeForBlocks
    (\case
      Left err -> logInfo $ T.pack err
      Right blk -> do
        print blk
        saveBlockHeader pgPool blk
    )
    (readMVar interruptFlag >> sleep 3 >> return True)



getArgs :: (String -> IO ()) -> IO ()
getArgs main' = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> main' configPath
    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


-- FIXME: handle possible PG error
saveBlockHeader :: Pool PG.Connection -> BlockHeader -> IO ()
saveBlockHeader pg (BlockHeader{..}) = do
  void $ liftIO $ withResource pg $ \c -> PG.execute c
    [sql| insert into block (currency, height, num_of_transactions, hash)
      values (?, ?, ?, ?)
    |]
    (currency, height, nTx, hash)
