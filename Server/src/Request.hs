{-# LANGUAGE OverloadedStrings #-}
module Request (getHeader) where
    
import Network.Wai
import Data.List

-- Get a given value for a header (if it exists)
getHeader :: Request -> String -> Maybe String
getHeader req name = case find (\(headerName, _) -> show headerName == name) (requestHeaders req) of 
    Just (_, value) -> Just (show value)