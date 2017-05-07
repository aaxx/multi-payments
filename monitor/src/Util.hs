
module Util where

import           Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LB
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.IO (stderr)


sleep :: Int -> IO ()
sleep seconds = threadDelay $ seconds * 10^(6 :: Int)

logInfo :: Text -> IO ()
logInfo = T.hPutStrLn stderr

txt :: Text -> Text
txt = id

json :: [(Text, Aeson.Value)] -> LB.ByteString
json = Aeson.encode . Aeson.object
