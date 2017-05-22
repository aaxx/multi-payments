
{-# LANGUAGE QuasiQuotes #-}

module BlockchainInfo where

import           Control.Concurrent (forkIO)
import           Control.Monad (forever, void, join)
import           Control.Lens hiding ((.=))

import           Data.Aeson.Lens
import           Data.Monoid ((<>))
import           Data.Maybe (catMaybes)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.WebSockets as WS
import qualified Wuss

import Types
import Util



subscribeForBlocks :: (Either String BlockHeader -> IO ()) -> IO Bool -> IO ()
subscribeForBlocks handleResult continue
  = Wuss.runSecureClient  "ws.blockchain.info" 443 "/inv" loop
  where
    loop conn = do
      void . forkIO . forever
        $ parseBlockHeader <$> WS.receiveData conn
        >>= handleResult

      void . forkIO . forever $ do
        sleep 30
        WS.sendPing conn $ json ["op" .= txt "ping"]

      WS.sendTextData conn $ json ["op" .= txt "blocks_sub"]

      continue >>= \case
        True -> loop conn
        False -> WS.sendClose conn $ txt "See ya!"


parseBlockHeader :: LB.ByteString -> Either String BlockHeader
parseBlockHeader msg = maybe
  (Left $ "Malformed JSON" ++ show msg)
  Right
  $ do
    jsn <- Aeson.decode msg :: Maybe Aeson.Value
    "block" <- jsn ^? key "op"
    BlockHeader
      <$> pure "BTC"
      <*> (jsn ^? key "x" . key "hash" . _String)
      <*> (jsn ^? key "x" . key "height" . _Integral)
      <*> (jsn ^? key "x" . key "nTx" . _Integral)



blockDetails :: BlockHash -> IO (Either String BlockDetails)
blockDetails hash = do
  -- FIXME: share manager (via IORef or TVar)
  tlsMgr <- newManager tlsManagerSettings
  rq  <- parseRequest $ "https://blockchain.info/rawblock/" <> T.unpack hash
  rsp <- httpLbs rq tlsMgr
  let body = responseBody rsp
  -- FIXME: chk HTTP status
  return $ maybe (Left $ "Malformed JSON" ++ show body) Right $ do
    b <- Aeson.decode body :: Maybe Aeson.Value
    let mkOut o = (,)
          <$> (o ^? key "addr" . _String)
          <*> (o ^? key "value" . _Integral)
    let mkTx t = Tx
          <$> (t ^? key "hash" . _String)
          -- NB! skip outputs without address
          <*> (catMaybes . map mkOut . V.toList <$> (t ^? key "out" . _Array))
    BlockDetails
      <$> (BlockHeader
        <$> pure "BTC"
        <*> (b ^? key "hash" . _String)
        <*> (b ^? key "height" . _Integral)
        <*> (b ^? key "n_tx" . _Integral))
      <*> join (sequence . map mkTx . V.toList <$> (b ^? key "tx" . _Array))
