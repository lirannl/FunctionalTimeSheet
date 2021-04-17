{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request (getHeader, determineQuery, AppReq(..)) where

import Data.ByteString.UTF8
import Data.CaseInsensitive
import Data.List
import Data.Time
import Network.Wai
import Text.Read

-- Get a given value for a header (if it exists)
getHeader :: Request -> String -> Maybe String
getHeader req name =
  case find (\(headerName, _) -> headerName == mk (fromString name)) (requestHeaders req) of
    Just (_, value) -> Just (toString value)
    Nothing -> Nothing

data AppReq = Hours Int | Date Day | Unauthorised | Invalid

determineQuery :: Request -> AppReq
determineQuery req =
  case queryString req of
        [("date", Just dateString)] -> case parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (toString dateString) of
          Just (d :: Day) -> Date d
          _ -> Invalid
        [("hours", Just hoursStr)] -> case readMaybe (toString hoursStr) of
          Just (hours :: Int) -> Hours hours
          Nothing -> Invalid
        _ -> Invalid