module MyEvents where

import Data.Time
import Event -- relevant definitions are in Event.hs

-- | List of events, order does not matter. It will be sorted by the start time of the events.
myEvents :: [Event]
myEvents = 
  [ Event
      "Example Event 1"
      [ day @. int -- @. is used to combine a day with one time interval, 
                   -- (@.) :: Day -> TI -> LocalTimeInterval
      | day <- take 20 $ cycle [date 2024 7 1..date 2024 7 5] -- date is the synonym for fromGregorian
      , int <- [ TI 9 30   13 30  -- TI is a constructor with 4 fields, marks a time interval
               , TI 15 30  16 30 
               ]
      ] 
      -- you can also write:
      -- concat $ zipWith (@@) (take 20 $ cycle [date 2024 7 1..date 2024 7 5]) [TI 9 30  13 30, TI 15 30  16 30]
      -- @@ is used to combine a day with a list of time intervals
      -- (@@) :: Day -> [TI] -> [LocalTimeInterval]
      EuropeLondon
      (Info 
        "Example Location 1" 
        [ ("Example Event Website", "https://example.com") 
        , ("Useful reference", "https://example.com/reference")
        ]
      )

  , Event 
      "Some Every Two Week Seminar"
      [ day @. TI 13 10   15 10
      | day <- concat $ take 5 $ iterate (addDays 14 <$>) [date 2024 7 8..date 2024 7 12] 
      ]
      EuropeLondon
      (Info 
        "University of Someplace, Some Building, Some Room" 
        [("Seminar website", "https://someplace.edu/seminar")]
      )
  ]
