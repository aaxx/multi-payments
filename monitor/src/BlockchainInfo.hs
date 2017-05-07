
module BlockchainInfo where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forever, void, join)
import           Control.Lens hiding ((.=))

import           Data.Maybe (fromMaybe)
import           Data.Aeson.Lens
import           Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.WebSockets as WS
import qualified Wuss

import Util


blockchainInfo :: MVar () -> Pool PG.Connection -> IO ()
blockchainInfo intFlag pg = do
  let bcInfo = "ws.blockchain.info"
  Wuss.runSecureClient bcInfo 443 "/inv" $ \conn -> do
    void . forkIO . forever
      $ WS.receiveData conn >>= processBlock
        >>= maybe (logInfo "Malformed json")
          (saveBlockToPG pg)

    void . forkIO . forever $ do
      sleep 30
      WS.sendPing conn $ json ["op" .= txt "ping"]

    WS.sendTextData conn $ json ["op" .= txt "blocks_sub"]

    readMVar intFlag
    WS.sendClose conn $ txt "See ya!"


processBlock :: LB.ByteString -> IO (Maybe Aeson.Value)
processBlock msg = do
  let hash = do
        jsn <- Aeson.decode msg :: Maybe Aeson.Value
        "block" <- jsn ^? key "op"
        jsn ^? key "x" . key "hash" . _String
  fmap join $ sequence $ fmap detailedBlockInfo hash


detailedBlockInfo :: Text -> IO (Maybe Aeson.Value)
detailedBlockInfo hash = do -- logInfo hash >> return (Aeson.toJSON ())
  tlsMgr <- newManager tlsManagerSettings
  rq  <- parseRequest $ "https://blockchain.info/rawblock/" <> T.unpack hash
  rsp <- httpLbs rq tlsMgr
  -- FIXME: chk status
  return $ Aeson.decode $ responseBody rsp


saveBlockToPG :: Pool PG.Connection -> Aeson.Value -> IO ()
saveBlockToPG pg val = print val

