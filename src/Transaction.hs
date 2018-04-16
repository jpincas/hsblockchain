{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transaction
  ( Transaction(..)
  , handlerShowPending
  , handlerIncoming
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy   as B
import           Database.Redis         (Connection, lpush, runRedis, set)
import           GHC.Generics
import           Web.Scotty             (ActionM, json, jsonData)

-- TYPES
data Transaction = Transaction
  { sender    :: String
  , recipient :: String
  , amount    :: Int
  , fee       :: Int
  } deriving (Generic, Show)

instance FromJSON Transaction

instance ToJSON Transaction

data Error = Error
  { message :: String
  } deriving (Generic, Show)

instance FromJSON Error

instance ToJSON Error

-- FUNCTIONALITY
processIncoming :: Transaction -> Connection -> IO (Either Transaction Error)
processIncoming tx cx =
  case verifyTransaction tx of
    Just error -> return $ Right error
    Nothing    -> addToPending tx cx

verifyTransaction :: Transaction -> Maybe Error
verifyTransaction Transaction {amount = amount'}
  | amount' > 100 = Just $ Error "Amount too large"
  | otherwise = Nothing

addToPending :: Transaction -> Connection -> IO (Either Transaction Error)
addToPending tx cx =
  runRedis cx $ do
    lpush "pendingTransactions" [B.toStrict . encode $ tx]
    return $ Left tx

-- HANDLERS
handlerShowPending :: Connection -> ActionM ()
handlerShowPending _ =
  json Transaction {sender = "Jon", recipient = "Jess", amount = 100, fee = 0}

handlerIncoming :: Connection -> ActionM ()
handlerIncoming cx = do
  (tx :: Transaction) <- jsonData
  r <- liftIO $ processIncoming tx cx
  case r of
    Left tx     -> json tx
    Right error -> json error
-- PERSISTENCE
