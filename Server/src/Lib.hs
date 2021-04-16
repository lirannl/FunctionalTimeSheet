{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( entry,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Configuration.Dotenv.Environment
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.UTF8 (toString)
import Helper (definitelyString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Request
import Settings (Config (..), readSettings)

entry :: IO ()
entry = do
  maybeConfig <- do dotenv <- loadFile defaultConfig; env <- getEnvironment; return $ readSettings (env ++ dotenv)
  case maybeConfig of
    Just config -> do
      putStrLn $ "Now listening for incoming traffic on port " ++ show (port config)
      if debug config then putStrLn "Running in debug mode!" else putStr ""
      run (port config) (app config)
    Nothing -> putStrLn "Environment values missing or invalid. Please set them / supply them via .env"

app :: Config -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app config req respond = do
  if debug config
    then putStrLn $ "Recieved " ++ toString (requestMethod req) ++ " request from " ++ definitelyString (getHeader req "X-Forwarded-For")
    else putStr ""
  case determineQuery req of
    Just (Hours hours) ->
      let msg = "Submitted " ++ show hours ++ " hours"
       in do
            putStrLn msg
            respond $ responseLBS status200 [] (fromString msg)
    Just (Day day) ->
      let hours = 0
       in do
            respond $ responseLBS status200 [] (fromString $ show hours)
    Nothing -> respond $ responseLBS status400 [] "Invalid request"