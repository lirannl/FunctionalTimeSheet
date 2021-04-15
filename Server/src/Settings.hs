
{-# LANGUAGE NamedFieldPuns #-}
module Settings (readSettings, Config(..)) where

import System.Environment.MrEnv

data Config = Config {
    port :: Int,
    debug :: Bool
} deriving (Show)

readSettings :: IO Config
readSettings = do
    port <- envAsInt "PORT" 8080
    debug <- envAsBool "DEBUG" False
    return $ Config { port, debug }