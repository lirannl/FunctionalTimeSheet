{-# LANGUAGE ScopedTypeVariables #-}

module Helper (definitelyString, toString, customFormat, wrap, substr, startsWith) where

import Data.List
import Data.Maybe
import Text.Regex.TDFA

startsWith :: String -> String -> Bool
startsWith text str = text =~ ("^" ++ str)

definitelyString :: Maybe String -> String
definitelyString = fromMaybe ""

toString :: Show a => Either a String -> String
toString (Right str) = str
toString (Left other) = show other

wrapNegative :: Foldable t => Int -> t a -> Int
wrapNegative index list = if index < 0 then length list + index else index

wrap :: (Int -> [a] -> [a]) -> Int -> [a] -> [a]
wrap func index list = func (wrapNegative index list) list

substr :: Int -> Int -> String -> String
substr start end orig = wrap drop start (wrap take end orig)

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
customFormat substitutions orig =
  -- Start going through the parts of the text
  replaceMatches (wrap take (-1) parts) substitutions initialAccumulator
  where
    initialAccumulator = ""
    -- Split the text into parts
    parts = getAllTextMatches (orig =~ "({[^{}]*})|([^{}]*)")