{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Time.Clock
import           Database.Redis           (Connection, checkedConnect,
                                           defaultConnectInfo)
import           Lib
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Text.Pretty.Simple       (pPrint)
import           Transaction              (Transaction (..), handlerIncoming,
                                           handlerShowPending)
import           Web.Scotty               (ScottyM, get, post, scotty)

-- server :: Server API
-- server = createTransaction
-- createTransaction :: Transaction -> Handler Transaction
-- createTransaction t = return t
-- api :: Proxy API
-- api = Proxy
-- app :: Application
-- app = serve api server
-- main :: IO ()
-- main = do
--   run 8081 app
-- -- main :: IO ()
-- -- main = do
-- --   timeNow <- getCurrentTime
-- --   let chain = createChain 20 timeNow
-- --   pPrint chain
-- --   pPrint $ isValidChain chain
routes :: Connection -> ScottyM ()
routes cx = do
  post "/transactions" $ Transaction.handlerIncoming cx
  get "/transactions" $ Transaction.handlerShowPending cx

main :: IO ()
main = do
  cx <- checkedConnect defaultConnectInfo
  scotty 3000 (routes cx)
