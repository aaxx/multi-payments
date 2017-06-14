
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad (when, void)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.Map as Map
import           Data.List (foldl')

import           System.Environment (getEnv)

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Web.Scotty
import           Network.Wai.Middleware.Cors



const_HTTP_PORT :: Int
const_HTTP_PORT = 8000

const_curencies :: [Text]
const_curencies = ["BTC", "LTC", "XMR", "DASH"]

main :: IO ()
main = do
  pgConn <- PG.ConnectInfo
    <$> getEnv "RDS_HOSTNAME"
    <*> (read <$> getEnv "RDS_PORT")
    <*> getEnv "RDS_USERNAME"
    <*> getEnv "RDS_PASSWORD"
    <*> getEnv "RDS_DB_NAME"

  -- create pool & check PG connection
  pg <- createPool (PG.connect pgConn) PG.close 1 (fromInteger 20) 5
  [[True]] <- withResource pg $ flip PG.query_ [sql| select true |]

  logInfo "HTTP" const_HTTP_PORT
  scotty const_HTTP_PORT $ httpServer pg


-- TODO:
-- - cache
-- - check limits in invoice
-- - timeout in invoice

httpServer :: Pool PG.Connection -> ScottyM ()
httpServer pg = do
  let corsPolicy = simpleCorsResourcePolicy
  middleware $ cors (const $ Just corsPolicy)



  get "/" $ text "Ok"

  get "/config" $ do
    sendAnalytics pg
    [[res]] <- liftAndCatchIO $ withResource pg
      $ flip PG.query_ [sql| select ico_info() |]
    json (res :: Aeson.Value)

  get "/info/:ethAddr" $ do
    sendAnalytics pg
    ethAddr <- T.toLower <$> param "ethAddr"
    when (not $ validEth ethAddr) $ httpError "Invalid ETH address"

    txs <- liftAndCatchIO $ withResource pg
      $ \c -> PG.query c [sql|
          select tx.currency, row_to_json (tx.*)
            from transactions_by_addr tx
            where "snmAddr" = ?
        |] [ethAddr]
    let txMap = foldl'
          (\m (x :: Text, tx :: Aeson.Value) -> Map.insertWith' (++) x [tx] m)
          Map.empty txs

    json (Aeson.object
      [ "snmBalance" .= txt "0.00000000",
        "tx" .= Aeson.toJSON txMap
      ])


  post "/invoice/:curr/:ethAddr" $ do
    sendAnalytics pg
    currency <- T.toUpper <$> param "curr"
    when (not $ currency `elem` const_curencies)
      $ httpError "Invalid currency code"

    ethAddr  <- T.toLower <$> param "ethAddr"
    when (not $ validEth ethAddr) $ httpError "Invalid ETH address"

    res <- liftAndCatchIO $ withResource pg $ \c -> PG.query c
      [sql| select create_invoice(?, ?) |]
      (currency, ethAddr)

    case res of
      [[Just jsn]] -> json (jsn :: Aeson.Value)
      [[Nothing]]  -> httpError "Sudden lack of free addresses"
      _  -> do
        logError "invoice: unexpected query result" res
        raise "Unexpected query result"


-----------
-- Utility
-----------

txt :: Text -> Text
txt = id


sendAnalytics :: Pool PG.Connection -> ActionM ()
sendAnalytics pg = do
  cid <- param "cid" `rescue` \_ -> return ""
  when (cid /= "") $ do
    rq <- request
    void $ liftAndCatchIO $ withResource pg
      $ \c -> PG.execute c
        [sql| insert into analytics_event (cid, action) values (?,?) |]
        [cid , T.pack $ show rq]


validEth :: Text -> Bool
validEth eth = case T.hexadecimal eth of
  Right (_ :: Integer, "")
    -> "0x" `T.isPrefixOf` eth && T.length eth == 42
  _ -> False


logInfo :: (MonadIO m, Show a) => Text -> a -> m ()
logInfo m a = liftIO $ T.putStrLn $ m <> " >> " <> T.pack (show a)

logError :: (MonadIO m, Show a) => Text -> a -> m ()
logError m a = liftIO $ T.putStrLn $ "ERROR: " <> m <> " >> " <> T.pack (show a)

httpError :: Text -> ActionM ()
httpError msg = json (Aeson.object ["error" .= msg]) >> finish
