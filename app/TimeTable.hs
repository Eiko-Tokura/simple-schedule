module TimeTable where

import Data.Time
import Data.List
import Data.Char
import Event
import qualified Data.Map as M
import qualified Data.Set as S

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
  | otherwise = concat $
    ( [ ((0, dateWidth + hourWidth * (h - minHour)), show h)
      | h <- [minHour..maxHour]
      ]
    : [ ((r, 0), let (_,m,d) = toGregorian t in show m ++ "/" ++ show d)
        : [ ((r, dateWidth + hourWidth * (h - minHour)), "|")
          | h <- [minHour..maxHour]
          ]
      | (r,t) <- zip [1..] [st..ed]
      ]
    )
    ++
    [ [ ((r, dateWidth + s), fst $ brackets event)
      , ((r, dateWidth + s+1), shorten (e-s-1) (name event))
      , ((r, dateWidth + e), snd $ brackets event)
      ]
    | (r, eventsInRow) <- zip [1..] timeBlocks
    , (ts, te, event) <- eventsInRow
    , let s = (todHour ts - minHour) * hourWidth + (todMin ts * hourWidth) `div` 60
          e = (todHour te - minHour) * hourWidth + (todMin te * hourWidth) `div` 60
          brackets ev
            | OnlyDay _ <- head $ localTimes ev = (":", ":") --represent incomplete information
            | _ <- localTimes ev = ("[", "]")
    ]
    where hourWidth = 10
          dateWidth = 5
          minHour = minimum [ todHour ts' | eventsInRow' <- timeBlocks, (ts',_,_) <- eventsInRow' ]
          maxHour = maximum [ todHour te' | eventsInRow' <- timeBlocks, (_,te',_) <- eventsInRow' ]
          timeBlocks = toTimeBlocks events (st, ed)

allActiveEvents :: [Event] -> (Day, Day) -> [Event]
allActiveEvents events (st, ed) = S.toList $ S.fromList
  [ event
  | eventsInRow <- toTimeBlocks events (st, ed)
  , (_,_,event) <- eventsInRow
  ]

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

shorten :: Int -> String -> String
shorten n str = head $ filter ((<= n) . length)
  (  [ str ]
  ++ [ concatMap (take i . capitalizeHead) (words str) | i <- [20,19..1] ]
  ++ [ take n str ] -- proved to return, so it's safe to use head
  )
  where 
    capitalizeHead [] = []
    capitalizeHead (c:cs) = toUpper c : cs

type StringImage = [((Int, Int), String)]
-- this is used to render and represent a 2D image. Note that we enforce that the latter terms can override the former terms.
type CharImage = M.Map (Int, Int) Char   

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

renderStringImage :: StringImage -> String
renderStringImage = renderCharImage . stringImageToCharImage
