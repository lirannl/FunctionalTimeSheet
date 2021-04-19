{-# LANGUAGE ScopedTypeVariables #-}

module Helper (definitelyString, toString, customFormat, takeWrap, dropWrap, substr, startsWith) where

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Text.Regex.TDFA

startsWith :: String -> String -> Bool
startsWith text str = text =~ ("^" ++ str)

definitelyString :: Maybe String -> String
definitelyString = fromMaybe ""

toString :: Show a => Either a String -> String
toString (Right str) = str
toString (Left other) = show other

wrapNegative i str = if i < 0 then length str + i else i

takeWrap i f = take (wrapNegative i f) f

dropWrap i f = drop (wrapNegative i f) f

substr :: Int -> Int -> String -> String
substr start end orig = dropWrap start (takeWrap end orig)

replaceMatches :: [String] -> [(String, String)] -> String -> Maybe String
replaceMatches [] substitutions acc = Just acc
replaceMatches (part : otherParts) substitutions acc =
  if part =~ "{.*}"
    then case find (\(origText, _) -> substr 1 (-1) part == origText) substitutions of
      -- Move the destination of the relevant substitution to the end of the accumulating string
      Just (origText, replacement) -> replaceMatches otherParts (filter (\(orig, _) -> origText /= orig) substitutions) (acc ++ replacement)
      -- Replacement failed - all fields in the text must match a substitution
      Nothing -> Nothing
    else -- Move the current part to the end of the accumulating string
      replaceMatches otherParts substitutions (acc ++ part)

customFormat :: [(String, String)] -> String -> Maybe String
customFormat [] orig = Just orig
customFormat substitutions orig =
  -- Start going through the parts of the text
  replaceMatches (takeWrap (-1) parts) substitutions initialAccumulator
  where
    initialAccumulator = ""
    -- Split the text into parts
    parts = getAllTextMatches (orig =~ "({[^{}]*})|([^{}]*)") :: [String]