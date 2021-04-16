module Transactions () where

import Database.MongoDB
import Data.Time

getHoursForDay :: Day -> Action IO Int
getHoursForDay day = _