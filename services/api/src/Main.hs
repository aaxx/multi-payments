
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad (when)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))

import           System.Environment (getEnvironment, getEnv)

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Web.Scotty
import           Network.Wai.Middleware.Cors
import           Network.HTTP.Types.Status (badRequest400)



const_HTTP_PORT :: Int
const_HTTP_PORT = 8000

const_curencies :: [Text]
const_curencies = ["BTC", "LTC", "ETC", "XMR", "DASH"]

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


httpServer :: Pool PG.Connection -> ScottyM ()
httpServer pg = do
  let corsPolicy = simpleCorsResourcePolicy
  middleware $ cors (const $ Just corsPolicy)

  get "/" $ text "Ok"

  get "/config" $ do
    [[res]] <- liftAndCatchIO $ withResource pg
      $ flip PG.query_ [sql| select ico_info() |]
    json (res :: Aeson.Value)

  get "/info/:ethAddr" $ do
    ethAddr <- T.toLower <$> param "ethAddr"
    when (not $ validEth ethAddr)
      $ status badRequest400 >> text "Invalid ETH address" >> finish

    [[res]] <- liftAndCatchIO $ withResource pg
      $ \c -> PG.query c [sql| select addr_info(?) |] [ethAddr]
    json (res :: Aeson.Value)

  post "/invoice/:curr/:ethAddr" $ do
    currency <- T.toUpper <$> param "curr"
    when (not $ currency `elem` const_curencies)
      $ status badRequest400 >> text "Invalid currency code" >> finish

    ethAddr  <- T.toLower <$> param "ethAddr"
    when (not $ validEth ethAddr)
      $ status badRequest400 >> text "Invalid ETH address" >> finish

    -- TODO: get referrer & session form cookies

    res <- liftAndCatchIO $ withResource pg $ \c -> PG.query c
      [sql| select create_invoice(?, ?) |]
      (currency, ethAddr)

    case res of
      [[Just jsn]] -> json (jsn :: Aeson.Value)
      [[Nothing]] -> json (Aeson.object ["error" .= txt "Sudden lack of free addresses"])
      _  -> do
        logError "invoice: unexpected query result" res
        raise "Unexpected query result"

  get "/env"
    $ liftIO getEnvironment
    >>= json . Aeson.object
      . map (\(k,v) -> T.pack k .= T.pack v)


-----------
-- Utility
-----------

txt :: Text -> Text
txt = id


validEth :: Text -> Bool
validEth eth = case T.hexadecimal eth of
  Right (_ :: Integer, "")
    -> "0x" `T.isPrefixOf` eth && T.length eth == 42
  _ -> False


logInfo :: (MonadIO m, Show a) => Text -> a -> m ()
logInfo m a = liftIO $ T.putStrLn $ m <> " >> " <> T.pack (show a)


logError :: (MonadIO m, Show a) => Text -> a -> m ()
logError m a = liftIO $ T.putStrLn $ "ERROR: " <> m <> " >> " <> T.pack (show a)
