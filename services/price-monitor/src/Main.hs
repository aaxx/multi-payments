
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main where

import           Control.Exception (SomeException, handle)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Monad

import           Data.Aeson.Lens
import qualified Data.Aeson as Aeson
import           Control.Lens

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import           Data.Pool (createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           System.Environment (lookupEnv)

import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as HS
import qualified Network.HTTP.Types.Status as HTTP



trackedCurrencies :: [Text]
trackedCurrencies = ["LTC", "ETH", "ETC", "DASH", "XMR", "TIME"]


main :: IO ()
main = do
  logInfo "Start" ()
  loopInterval <- maybe 666 read <$> lookupEnv "LOOP_INTERVAL_SECONDS"

  -- create pool & check PG connection
  pgPool <- createPool (PG.connectPostgreSQL "") PG.close 1 (fromInteger 20) 5
  [[True]] <- withResource pgPool $ flip PG.query_ [sql| select true |]

  tlsMgr <- H.newManager HS.tlsManagerSettings
  rq     <- H.parseRequest "https://api.coinmarketcap.com/v1/ticker/?limit=100"

  thread <- async $ forever $ do
    handle (\e -> logError "Exception catched" (e :: SomeException)) $ do
      H.httpLbs rq tlsMgr >>= \case
        rsp | H.responseStatus rsp /= HTTP.status200
            -> logError "Unexpected response" rsp
        rsp -> do
          let Just body = Aeson.decode $ H.responseBody rsp :: Maybe [Aeson.Value]
          let prices
                = [ (sym, priceTxt)
                  | x <- body
                  , let Just sym = x ^? key "symbol" . _String
                  , let Just priceTxt =  x ^? key "price_btc" . _String
                  , let Right (_price :: Double, "") = T.rational priceTxt
                  , sym `elem` trackedCurrencies
                  ]
          logInfo "Coinmarcetcap" prices
          void $ withResource pgPool $ \c -> PG.executeMany c
            [sql| insert into price (currency, price) values (?,?) |]
            prices

    threadDelay $ loopInterval * 1000 * 1000

  void $ wait thread


-----------
-- Utility
-----------

logInfo :: Show a => Text -> a -> IO ()
logInfo m a = T.putStrLn $ m <> " >> " <> T.pack (show a)

logError :: Show a => Text -> a -> IO ()
logError m a = T.putStrLn $ "ERROR: " <> m <> " >> " <> T.pack (show a)
