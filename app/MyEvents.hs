module MyEvents where

import Data.Time
import Event

-- | List of events. 

myEvents :: [Event]
myEvents = 
  [ Event
      "Example Event 1"
      [ SameDayInterval day int
      | day <- take 20 $ cycle [fromGregorian 2024 7 1..fromGregorian 2024 7 5]
      , int <- [ (TimeOfDay 9 30 0, TimeOfDay 13 30 0)
               , (TimeOfDay 15 30 0, TimeOfDay 16 30 0)
               ]
      ]
      EuropeLondon
      (Info 
        "Example Location 1" 
        [ ("Example Event 1", "https://example.com") 
        , ("Useful reference", "https://example.com/reference")
        ]
      )

  , Event 
      "Some Every Two Week Seminar"
      [ SameDayInterval day (TimeOfDay 13 10 0, TimeOfDay 15 10 0)
      | day <- concat $ take 5 $ iterate (addDays 14 <$>) [fromGregorian 2024 7 8..fromGregorian 2024 7 12] 
      ]
      EuropeLondon
      (Info 
        "University of Someplace, Some Building, Some Room" 
        [("Seminar website", "https://someplace.edu/seminar")]
      )
  ]
