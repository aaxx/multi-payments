
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad.IO.Class (liftIO)

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

import           Web.Scotty


main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      conf <- Config.load [Config.Required configPath]

      logInfo $ "Reading config from " <> T.pack configPath

      httpPort  <- Config.require conf "http.port"
      pgHost    <- Config.require conf "pg.host"
      pgPort    <- Config.require conf "pg.port"
      pgUser    <- Config.require conf "pg.user"
      pgPwd     <- Config.require conf "pg.pass"
      pgDb      <- Config.require conf "pg.db"

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

      scotty httpPort
        $ httpServer pgPool

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


httpServer :: Pool PG.Connection -> ScottyM ()
httpServer pgPool = do

  post "/invoice/:curr/:ethAddr" $ do
    currency <- txt <$> param "curr"
    ethAddr  <- txt <$> param "ethAddr"

    -- FIXME: chk currency
    -- FIXME: chk address format (& checksum)
    -- FIXME: normalize address

    res <- liftIO $ withResource pgPool $ \c -> PG.query c
      [sql|
        insert into invoice (currency, from_addr, to_eth_addr)
          select a.currency, a.addr, ?
            from address a left join invoice i on (a.addr = i.from_addr)
            where a.currency = ?
              and (to_eth_addr is null or to_eth_addr = ?)
            order by to_eth_addr nulls last
            limit 1
          on conflict on constraint invoice_unique_addr
            do update set last_activity = now()
          returning currency::text, from_addr, to_eth_addr
      |]
      (ethAddr, currency, ethAddr)

    case res of
      [[crncy, addr, eth_addr]] ->
        json (Aeson.object
          [ "currency" .= txt crncy
          , "addr"     .= txt addr
          , "eth_addr" .= txt eth_addr])
      [] -> json (Aeson.object ["error" .= txt "Sudden lack of free addresses"])
      _  -> error $ "Unexpected result " ++ show res


-----------
-- Utility
-----------

logInfo :: Text -> IO ()
logInfo = T.hPutStrLn stderr

txt :: Text -> Text
txt = id
