{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( entry,
  )
where

import qualified Data.ByteString as ByteS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Settings
import Helper
import Request

entry :: IO ()
entry = do
  config <- readSettings
  putStrLn $ "Now listening for incoming traffic on port " ++ show (port config)
  if debug config then putStrLn "Running in debug mode!" else putStr ""
  run (port config) app

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = do
  putStrLn $ "Recieved " ++ show (requestMethod req) ++ " request from " ++ definitelyString (getHeader req "X-Forwarded-For")
  respond $ responseLBS status200 [] "Hello!"