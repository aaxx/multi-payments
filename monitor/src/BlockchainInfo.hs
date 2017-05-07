
module BlockchainInfo where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forever, void)
import           Control.Lens hiding ((.=))

import           Data.Aeson.Lens
import           Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.Text as T

import qualified Network.WebSockets as WS
import qualified Wuss

import Util


blockchainInfo :: MVar () -> IO ()
blockchainInfo intFlag = do
  let bcInfo = "ws.blockchain.info"
  Wuss.runSecureClient bcInfo 443 "/inv" $ \conn -> do
    void . forkIO . forever $ do
      msg <- WS.receiveData conn
      case Aeson.decode msg of
        Nothing  -> logInfo $ "Malformed json" <> T.pack (show msg)
        Just jsn -> case jsn ^? key "op" of
          Just "block" -> processBlock jsn {- pg connection -}
          _ -> return ()

    void . forkIO . forever $ do
      sleep 30
      WS.sendPing conn $ json ["op" .= txt "ping"]

    -- subscribe for new blocks
    WS.sendTextData conn $ json ["op" .= txt "blocks_sub"]

    readMVar intFlag
    WS.sendClose conn $ txt "See ya!"


processBlock :: Aeson.Value -> IO ()
processBlock info = print info
