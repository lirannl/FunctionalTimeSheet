{-# LANGUAGE ScopedTypeVariables #-}

module Helper (definitely, definitelyString, toString, customFormat, takeWrap, dropWrap, substr) where

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Text.Regex.TDFA

day :: IO Day
day = do
  utctDay <$> getCurrentTime

definitely :: Maybe a -> a -> a
definitely (Just a) _ = a
definitely Nothing def = def

definitelyString :: Maybe String -> String
definitelyString string = definitely string ""

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
replaceMatches (x : xs) substitutions acc =
  if x =~ "{.*}"
    then case find (\(src, _) -> substr 1 (-1) x == src) substitutions of
      -- Move the destination of the relevant substitution to the end of the accumulating string
      Just (src, dst) -> replaceMatches xs (filter (\(orig, _) -> src /= orig) substitutions) (acc ++ dst)
      -- Replacement failed - all fields in the text must match a substitution
      Nothing -> Nothing
    else -- Move the current part to the end of the accumulating string
      replaceMatches xs substitutions (acc ++ x)

customFormat :: [(String, String)] -> String -> Maybe String
customFormat [] orig = Just orig
customFormat substitutions orig =
  -- Split the text into parts
  let parts = getAllTextMatches (orig =~ "({[^{}]*})|([^{}]*)") :: [String]
   in -- Start going through the parts of the text
      replaceMatches (takeWrap (-1) parts) substitutions ""