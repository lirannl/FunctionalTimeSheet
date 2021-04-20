{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Settings (readSettings, Config (..)) where

import Data.ByteString (ByteString, pack)
import Data.ByteString.UTF8 (fromString)
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as Text
import Helper
import Text.Hex (decodeHex)
import Text.Read

data Config = Config
  { port :: Int,
    debug :: Bool,
    mongoURL :: String,
    passwordHash :: ByteString,
    defaultPage :: String
  }
  deriving (Show)

type VarReader = String -> Maybe String

readFromEnv :: [(String, String)] -> VarReader
readFromEnv env string = case List.find (\(name, _) -> name == string) env of
  Just (_, value) -> Just value
  Nothing -> Nothing

parsePort :: VarReader -> Int
parsePort var =
  let def = 8080
   in case readMaybe (fromMaybe (show def) (var "PORT")) of
        Just (port :: Int) -> port
        _ -> def

-- Required environment variables in the order in which they must be read
variables = ["MONGO_USER", "MONGO_PASSWORD", "MONGO_URL", "DB_NAME", "PASSWORD_HASH", "DEFAULT_PAGE"]

readSettings :: [(String, String)] -> Maybe Config
readSettings env =
  let var = readFromEnv env
   in case mapM var variables of
        Just [mongoUsername, mongoPassword, url, dbName, passwordHashString, defaultPage] ->
          case (customFormat [("user", mongoUsername), ("password", mongoPassword), ("dbName", dbName)] url, decodeHex $ Text.pack passwordHashString) of
            (Just mongoURL, Just passwordHash) ->
              Just
                Config
                  { port = parsePort var,
                    debug = case var "DEBUG" of Nothing -> False; Just "" -> False; Just _ -> True,
                    ..
                  }
            _ -> Nothing
        _ -> Nothing