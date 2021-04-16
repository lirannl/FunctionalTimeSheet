{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Settings (readSettings, Config (..)) where

import Data.List
import Text.Read
import Helper

data Config = Config
  { port :: Int,
    debug :: Bool,
    mongoURL :: String
  }
  deriving (Show)

type VarReader = String -> Maybe String

readFromEnv :: [(String, String)] -> VarReader
readFromEnv env string = case find (\(name, _) -> name == string) env of
  Just (_, value) -> Just value
  Nothing -> Nothing

parsePort :: VarReader -> Int
parsePort var =
  let def = 8080
   in case var "PORT" of
        Just portStr -> case readMaybe portStr of
          Just (port :: Int) -> port
          Nothing -> def
        Nothing -> def

readSettings :: [(String, String)] -> Maybe Config
readSettings env =
  let var = readFromEnv env
   in case mapM var ["MONGO_USER", "MONGO_PASSWORD", "MONGO_URL", "DB_NAME"] of
        Just [mongoUsername, mongoPassword, url, dbName] ->
          case customFormat [("user", mongoUsername), ("password", mongoPassword), ("dbName", dbName)] url of
            Nothing -> Nothing
            Just mongoURL ->
              Just
              Config
                { port = parsePort var,
                debug = case var "DEBUG" of Nothing -> False; Just "" -> False; Just _ -> True,
                ..
                }
        Nothing -> Nothing
