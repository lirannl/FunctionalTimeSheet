{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transactions (getDataForDay, saveWork) where

import qualified Data.Bson as Bson
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Database.MongoDB
  ( Action,
    Document,
    Field ((:=)),
    Select (select),
    Val (val),
    find,
    findOne,
    insert,
    merge,
    replace,
    rest,
    (=:),
  )
import Text.Read (readMaybe)

data Doc = Doc
  { hours :: Int,
    amount :: Int
  }
  deriving (Show)

addDocs :: Doc -> Doc -> Doc
addDocs doc1 doc2 = Doc {hours = sum (map hours [doc1, doc2]), amount = sum (map amount [doc1, doc2])}

getData :: Document -> Doc
getData doc = Doc {hours = hours, amount = amount}
  where
    (hours :: Int) = fromMaybe 0 (Bson.lookup "hours" doc)
    (amount :: Int) = fromMaybe 0 (Bson.lookup "amount" doc)

getInt :: Document -> Text -> Int
getInt doc fieldName = fromMaybe 0 (Bson.lookup fieldName doc)

getDataForDay :: Day -> Action IO Doc
getDataForDay day = do
  selections <- rest =<< find (select ["time" =: UTCTime day 0] "timesheet")
  return $
    foldl
      -- Add all found documents into one
      addDocs
      -- Start from 0 hours and 0 pay
      (Doc {hours = 0, amount = 0})
      -- Take the BSON and turn it into a Doc record
      ( map
          (\document -> Doc {hours = getInt document "hours", amount = getInt document "amount"})
          selections
      )

saveWork :: Int -> Maybe Int -> Day -> Action IO ()
saveWork n specificAmount day =
  let time = UTCTime day 0
      income = case specificAmount of Just amount -> amount; Nothing -> n * 35
   in do
        --possibleDoc <- findOne (select ["time" =: time] "timesheet")
        --case possibleDoc of
        -- Update the existing record
        --Just doc -> do
        --result <- replace doc ["hours" =: (getInt doc "hours" + n), "amount" =: (getInt doc "hours" + income)]
        --return ()
        -- Insert a new record
        --Nothing -> do
        insert "timesheet" ["time" =: time, "hours" =: n, "amount" =: income]
        return ()