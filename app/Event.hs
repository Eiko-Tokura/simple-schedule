module Event where

import Data.Time

data LocalTimeZone  -- defined but relevant functions have not yet been implemented 
  = EuropeLondon | AsiaShanghai | AmericaNewYork | FixedTimeZone TimeZone
  deriving (Eq, Ord, Show)

data LocalTimeInterval
  = SameDayInterval
    { localDayOfInterval :: Day                    -- ^ Day of the event
    , timeOfDayInterval  :: (TimeOfDay, TimeOfDay) -- ^ Start and end time of the event
    }
  | OnlyDay Day                                    -- ^ Event lasts the whole day or no specific time is given
  deriving (Eq, Ord, Show)

type Link = String
data Info = NoInfo
  | Info
  { location :: String           -- ^ Location of the event
  , links    :: [(String, Link)] -- ^ Links to the event with their names
  }
  deriving (Eq, Ord, Show)

data Event = Event
  { name          :: String              -- ^ Event name
  , localTimes    :: [LocalTimeInterval] -- ^ Local times of the event
  , localTimeZone :: LocalTimeZone       -- ^ Local time zone of the event
  , additional    :: Info                -- ^ Additional information
  }
  deriving (Eq, Show)

instance Ord Event where
  e1 <= e2 = minimum (localTimes e1) <= minimum (localTimes e2) 

