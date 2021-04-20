{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request (getHeader, determineQuery, appPolicy, AppReq (..)) where

import Crypto.Hash.SHA256
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (fromString, toString)
import Data.CaseInsensitive
import Data.List (find)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics
import Helper (startsWith)
import Network.Wai
import Network.Wai.Middleware.Static
import Settings
import Text.Read

appPolicy :: Config -> Policy
appPolicy config =
  -- Don't match any path starting with "api"
  predicate (not . (`startsWith` "api/")) >-> predicate (/= "api") >-> serve (defaultPage config)
  where
    serve (path :: FilePath) = policy (\_ -> Just path)

-- Get a given value for a header (if it exists)
getHeader :: Request -> String -> Maybe String
getHeader req name =
  case find (\(headerName, _) -> headerName == mk (fromString name)) (requestHeaders req) of
    Just (_, value) -> Just (toString value)
    Nothing -> Nothing

data AppReq = HoursSubmission (Int, Maybe Int) | DateQuery Day | Unauthorised Bool | Invalid

data Body = Body
  { hours :: Int,
    amount :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Process unauthorised request
determineQuery :: Config -> Request -> ByteString -> AppReq
determineQuery config req rawBody =
  case find (\(title, _) -> title == "Authorization") (requestHeaders req) of
    Just (_, secret) ->
      -- If the hash of the provided secret matches
      if hash secret == passwordHash config
        then -- Request is authorised - process it
          determineAuthorisedQuery req rawBody
        else Unauthorised True
    Nothing -> Unauthorised False

-- Process requests after authorisation
determineAuthorisedQuery :: Request -> ByteString -> AppReq
determineAuthorisedQuery req rawBody =
  case (requestMethod req, queryString req) of
    (methodGet, [("date", Just dateString)]) -> case parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (toString dateString) of
      Just (d :: Day) -> DateQuery d
      _ -> Invalid
    (methodPost, _) -> case decode $ fromStrict rawBody of
      Just Body {hours, amount} -> HoursSubmission (hours, amount)
      _ -> Invalid