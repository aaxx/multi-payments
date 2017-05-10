
module Types where

import Data.Text (Text)


type BlockHash = Text

data BlockHeader = BlockHeader
  { currency :: Text
  , hash :: BlockHash
  , height :: Int
  , nTx :: Int
  } deriving Show

data BlockDetails = BlockDetails
  { header :: BlockHeader
  , transactions :: [Tx]
  } deriving Show

data Tx = Tx
  { txHash :: Text
  , txOuts :: [(Text, Integer)] -- (Address, Value)
  } deriving Show
