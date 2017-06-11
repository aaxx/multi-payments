
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad.IO.Class (liftIO)

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))

import           System.Environment (getEnvironment, getEnv)

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Web.Scotty


const_HTTP_PORT :: Int
const_HTTP_PORT = 8000


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
  get "/" $ text "Ok"

  get "/config" $ do
    [[res]] <- liftIO $ withResource pg
      $ flip PG.query_ [sql| select ico_info() |]
    json (res :: Aeson.Value)

  get "/env"
    $ liftIO getEnvironment
    >>= json . Aeson.object
      . map (\(k,v) -> T.pack k .= T.pack v)


-----------
-- Utility
-----------

txt :: Text -> Text
txt = id

logInfo :: Show a => Text -> a -> IO ()
logInfo m a = T.putStrLn $ m <> " >> " <> T.pack (show a)

logError :: Show a => Text -> a -> IO ()
logError m a = T.putStrLn $ "ERROR: " <> m <> " >> " <> T.pack (show a)
