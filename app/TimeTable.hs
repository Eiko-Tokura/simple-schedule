module TimeTable where

import Data.Time
import Data.List
import Data.Char
import Event
import qualified Data.Map as M
import qualified Data.Set as S

-- | Generate time blocks for the events happening between the given days.
toTimeBlocks :: [Event] -> (Day, Day) -> [[(TimeOfDay, TimeOfDay, Event)]]
toTimeBlocks events interval = 
  [ sortOn (\(s,_,_) -> s) 
        [ (start, end, event)
        | event <- events 
        , SameDayInterval dayOfInt (start, end) <- localTimes event
        , day == dayOfInt
        ]
    ++  
        [ (start', end', event)
        | event <- events 
        , OnlyDay dayOfInt <- localTimes event
        , day == dayOfInt
        , let (start', end') = (TimeOfDay 10 0 0, TimeOfDay 16 0 0)
        ]
  | day <- [fst interval..snd interval]
  ]

-- | Given a list of events, show the timetable for the events happening between the given days. printing each day in a row, with event times wrapped in [ event name ].
showTimeTable 
  :: [Event]         -- ^ List of events
  -> (Day, Day)      -- ^ Interval of days
  -> StringImage
showTimeTable events (st, ed)
  | all null timeBlocks = [ ((0, 0), "No events from " ++ show st ++ " to " ++ show ed ++ " owo!") ]
  | otherwise = 
    hourAndDates ++ concat
    [ concat 
      [ [((r, dateWidth + s), "[") | brackets event ]
      , [((r, dateWidth + s+1), shorten (e-s-1) (name event))]
      , [((r, dateWidth + e), "]") | brackets event ]
      ]
    | (r, eventsInRow) <- zip [1..] timeBlocks
    , (ts, te, event) <- eventsInRow
    , let s = (todHour ts - minHour) * hourWidth + (todMin ts * hourWidth) `div` 60
          e = (todHour te - minHour) * hourWidth + (todMin te * hourWidth) `div` 60
          brackets ev
            | OnlyDay _ <- head $ localTimes ev = False
            | _ <- localTimes ev = True
    ]
    where hourWidth = 10
          dateWidth = 5
          minHour = minimum [ todHour ts' | eventsInRow' <- timeBlocks, (ts',_,_) <- eventsInRow' ]
          maxHour = maximum [ todHour te' | eventsInRow' <- timeBlocks, (_,te',_) <- eventsInRow' ]
          timeBlocks = toTimeBlocks events (st, ed)

          hourAndDates :: StringImage
          hourAndDates 
              =  [ ((0, dateWidth + hourWidth * (h - minHour)), show h) | h <- [minHour..maxHour] ]
                   -- this first line shows the hours

              ++ [ ((r, 0), show m ++ "/" ++ show d) 
                 | (r, t) <- zip [1..] [st..ed]
                 , let (_, m, d) = toGregorian t 
                 ] -- this column shows the dates 

              ++ [ ((r, dateWidth + hourWidth * (h - minHour)), markWeekend w)
                 | (r, t) <- zip [1..] [st..ed]
                 , let w = dayOfWeek t
                 , h <- [minHour..maxHour]
                 ] -- these columns are the dashed lines seperating the hours

          markWeekend Saturday = "." -- weekend uses a dot to separate the hours
          markWeekend Sunday   = "." 
          markWeekend _        = ":" -- weekdays use a colon to separate the hours

-- | Filtering the list of events happening between the given days.
allActiveEvents :: [Event] -> (Day, Day) -> [Event]
allActiveEvents events (st, ed) = S.toList $ S.fromList
  [ event
  | eventsInRow <- toTimeBlocks events (st, ed)
  , (_,_,event) <- eventsInRow
  ]

-- | Given a list of events, show the information for the active events happening between the given days.
showInfos :: [Event] -> (Day, Day) -> String
showInfos events (st, ed) = intercalate "\n\n"
  [ intercalate "\n"
      [ name event
      , "  @" ++ location (additional event)
      , intercalate "\n" [ "  " ++ nameL ++ ": " ++ link | (nameL, link) <- links (additional event) ]
      ]
  | event <- allActiveEvents events (st, ed)
  , NoInfo /= additional event -- prevent error when there is no additional information
  ]

-- | Shorten a string to within a given length by abbreviating each word.
shorten :: Int -> String -> String
shorten n str = head $ filter ((<= n) . length)
  (  [ str ]
  ++ [ concatMap (take i . capitalizeHead) (words str) | i <- [20,19..1] ]
  ++ [ take n str ] -- proved to return, so it's safe to use head
  )
  where
    capitalizeHead [] = []
    capitalizeHead (c:cs) = toUpper c : cs

-- | this type is used to render and represent a 2D image. Note that we enforce that the latter terms can override the former terms.
type StringImage = [((Int, Int), String)]

type CharImage = M.Map (Int, Int) Char   

-- | Convert a StringImage to a CharImage
stringImageToCharImage :: StringImage -> CharImage
stringImageToCharImage simg = M.fromList $ concatMap f simg
  where f ((r, c), str) = [ ((r, c+i), ch) | (i, ch) <- zip [0..] str ]

-- | Render a CharImage to a string
renderCharImage :: CharImage -> String
renderCharImage img = intercalate "\n"
  [ [ M.findWithDefault ' ' (r, c) img
    | c <- [0..maxCol]
    ]
  | r <- [0..maxRow]
  ]
  where
    maxRow = maximum (map fst (M.keys img))
    maxCol = maximum (map snd (M.keys img))

-- | Render a StringImage to a string
renderStringImage :: StringImage -> String
renderStringImage = renderCharImage . stringImageToCharImage

