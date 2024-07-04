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

-- | The following are helper functions for defining time intervals in MyEvents.hs more concisely.

data TI = 
  TI  -- ^ This represents a time interval from (hour1,min1) to (hour2,min2).
  Int -- ^ hour1
  Int -- ^ min1
  Int -- ^ hour2
  Int -- ^ min2

-- | convert a time interval TI to a pair of TimeOfDay.
ti2intv :: TI -> (TimeOfDay, TimeOfDay)
ti2intv (TI h1 m1 h2 m2) = (TimeOfDay h1 m1 0, TimeOfDay h2 m2 0)

-- | synonym for a day represented by year, month, and day.
date :: Year -> MonthOfYear -> DayOfMonth -> Day
date = fromGregorian

dateIntvs :: Year -> MonthOfYear -> DayOfMonth -> [TI] -> [LocalTimeInterval]
dateIntvs y m d ints = 
  [ SameDayInterval (date y m d) intv | intv <- ti2intv <$> ints ]

-- | The operator used to combine a day and a time interval.
(@.) :: Day -> TI -> LocalTimeInterval
day @. int = SameDayInterval day (ti2intv int)

-- | The operator used to combine a day and a list of time intervals.
(@@) :: Day -> [TI] -> [LocalTimeInterval]
day @@ ints = [ SameDayInterval day int | int <- ti2intv <$> ints ]

-- | This function is used to generate a weekly repeating event.
repeatWeeklyBetween
    :: (Day, Day) -> [DayOfWeek] -> [TI] -> [LocalTimeInterval]
repeatWeeklyBetween (start, end) days times =
  concatMap (\d -> map (d @.) times) $
    takeWhile (<= end) $
      dropWhile (< start) $
        filter (\d -> dayOfWeek d `elem` days) [start..end]
