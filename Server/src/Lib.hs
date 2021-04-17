{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( entry,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Configuration.Dotenv.Environment
import Data.ByteString.UTF8 (toString)
import Data.Time
import Database.MongoDB
import GHC.Exts
import Helper (definitelyString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Request
import Settings (Config (..), readSettings)
import Transactions

entry :: IO ()
entry = do
  maybeConfig <- do dotenv <- loadFile defaultConfig; env <- getEnvironment; return $ readSettings (env ++ dotenv)
  case maybeConfig of
    Nothing -> putStrLn "Environment values missing or invalid. Please set them / supply them via .env"
    Just config -> do
      putStrLn $ "Now listening for incoming traffic on port " ++ show (port config)
      if debug config then putStrLn "Running in debug mode!" else putStr ""
      run (port config) (app config)

app :: Config -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app config req respond = do
  -- Connect to mongoDB
  pipe <- connect $ host "127.0.0.1"
  if debug config
    then putStrLn $ "Recieved " ++ toString (requestMethod req) ++ " request from " ++ definitelyString (getHeader req "X-Forwarded-For")
    else putStr ""
  case determineQuery config req of
    Unauthorised withPass -> respond $ responseLBS (if withPass then status401 else status403) [] "Unauthorised"
    Hours hours ->
      do
        time <- getCurrentTime
        access pipe master "timesheet" (saveHours hours (utctDay time))
        respond $ responseLBS status200 [] (fromString $ "Submitted " ++ show hours ++ " hours on " ++ show (utctDay time))
    Date day ->
      do
        hours <- access pipe master "timesheet" (getHoursForDay day)
        respond $ responseLBS status200 [] (fromString $ show hours)
    Invalid -> respond $ responseLBS status400 [] "Invalid request"