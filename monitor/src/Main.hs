
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad (void, forever, forM_)
import           Control.Concurrent.MVar
import           Control.Concurrent (forkIO)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Configurator as Config
import           Data.List.Split (chunksOf)

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified System.Environment as Env
import qualified System.Posix.Signals as Signal

import Types
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

  pg <- createPool (PG.connect cInfo) PG.close
      1 -- number of distinct sub-pools
        -- time for which an unused resource is kept open
      (fromInteger 20) -- seconds
      5 -- maximum number of resources to keep open

  -- handle interrupts gracefully
  interruptFlag <- newEmptyMVar :: IO (MVar ())
  let handleSig = Signal.Catch $ putMVar interruptFlag ()
  void $ Signal.installHandler Signal.sigINT handleSig Nothing
  void $ Signal.installHandler Signal.sigTERM handleSig Nothing

  -- double `forever` is to use fresh connection in case of exception
  void $ forkIO $ forever $ do
    updateTransactions pg
    withResource pg $ \c -> forever $ do
      void $ PG.execute_ c [sql| listen new_block |]
      PG.getNotification c >>= \case
        PG.Notification _ "new_block" "BTC" -> updateTransactions pg
        _ -> return ()

  -- FIXME: forever reconnect
  logInfo "Get blocks from blockchain.info"
  subscribeForBlocks
    (\case
      Left err -> logInfo $ T.pack err
      Right blk -> do
        print blk
        saveBlockHeader pg blk
    )
    (readMVar interruptFlag >> sleep 3 >> return False)



getArgs :: (String -> IO ()) -> IO ()
getArgs main' = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> main' configPath
    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"



-- FIXME: handle possible PG error
saveBlockHeader :: Pool PG.Connection -> BlockHeader -> IO ()
saveBlockHeader pg (BlockHeader{..}) = do
  void $ withResource pg $ \c -> PG.execute c
    [sql| insert into block (currency, height, num_of_transactions, hash)
      values (?, ?, ?, ?)
    |]
    (currency, height, nTx, hash)
  -- FIXME: use height to check for forks
  -- (update set deprecated = true)



updateTransactions :: Pool PG.Connection -> IO ()
updateTransactions pg = do
  freshBlocks <- withResource pg $ flip PG.query_
    [sql|
      select hash from block
        where not deprecated
          and not exists (select 1 from transaction where hash = block_hash)
    |]

  -- FIXME: handle errors
  -- FIXME: beware of stalled HTTP queries
  forM_ freshBlocks $ \[blkHash] -> do
    logInfo $ "Fetch block details for " <> blkHash
    blockDetails blkHash >>= \case
      Left err -> logInfo $ T.pack err
      Right BlockDetails{..} -> do
        let outs =
              [ ("BTC" :: Text, blkHash, txHash tx, addr, val)
              | tx <- transactions
              , (addr, val) <- txOuts tx
              ]
        forM_ (chunksOf 512 outs) $ \chunk ->
          -- FIXME: check result == length chunk
          void $ withResource pg $ \c -> PG.executeMany c
            [sql|
              insert into transaction (currency, block_hash, tx_hash, to_addr, value)
                values (?, ?, ?, ?, ?)
            |]
            chunk
