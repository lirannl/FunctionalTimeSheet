{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transactions (getHoursForDay, saveHours) where

import qualified Data.Bson as Bson
import Data.Maybe
import Data.Time
import Database.MongoDB
import Debug.Trace
import Text.Read (readMaybe)

getInt :: Document -> Int
getInt doc =
  let value = Bson.lookup "hours" doc
   in fromMaybe 0 value

getHoursForDay :: Day -> Action IO Int
getHoursForDay day = do
  selections <- rest =<< find (select ["time" =: UTCTime day 0] "timesheet")
  return $ sum (map getInt selections)

saveHours :: Int -> Day -> Action IO ()
saveHours n day =
  let time = UTCTime day 0
   in do
        possibleDoc <- findOne (select ["time" =: time] "timesheet")
        case possibleDoc of
          -- Update the existing record
          Just doc -> do
            result <- save "timesheet" $ merge ["hours" := val (getInt doc + n)] doc
            return ()
          -- Insert a new record
          Nothing -> do
            result <- insert "timesheet" ["time" =: time, "hours" =: n]
            return ()