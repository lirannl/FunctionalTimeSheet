{-# LANGUAGE OverloadedStrings #-}

module Request (getHeader) where

import Data.CaseInsensitive
import Data.List
import Network.Wai
import Helper
import Data.ByteString.UTF8

-- Get a given value for a header (if it exists)
getHeader :: Request -> String -> Maybe String
getHeader req name =
  case find (\(headerName, _) -> headerName == mk (fromString name)) (requestHeaders req) of
    Just (_, value) -> Just (toString value)
    Nothing -> Nothing