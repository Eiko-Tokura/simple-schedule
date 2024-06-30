# simple-schedule

I use this simple schedule to keep track of my events because I keep forgetting them: the seminars and events are scattered around everywhere! And the functionalities provided by the usual calendars are extremely unweildy and painful to use. Therefore I decide to use a more flexible language to describe the events, which turns out to be much more convinient (at least for me). I hope this helps you too owo!

With this simple schedule program (which is highly modifiable), you can manage your generation of events using directly haskell code, which provides increadible expressiveness and flexibility. 

Modify the MyEvents.hs file to add your events, an example may look like this

```haskell
myEvents :: [Event]
myEvents = 
  [ Event
      "Example Event 1"
      [ day @. int -- @. is used to combine a day with one time interval, 
                   -- (@.) :: Day -> TI -> LocalTimeInterval
      | day <- [date 2024 7 1..date 2024 7 5] -- date is the synonym for fromGregorian
      , int <- [ TI 9 30   13 30  -- TI is a constructor with 4 fields, marks a time interval
               , TI 15 30  16 30 
               ]
      ] 
      -- you can also write:
      -- concat $ zipWith (@@) [date 2024 7 1..date 2024 7 5] (repeat [TI 9 30  13 30, TI 15 30  16 30])
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
```

Then use cabal run or stack or ghc to complile and run the program. 

```bash
git clone https://github.com/Eiko-Tokura/simple-schedule
cd simple-schedule
cabal run -- SimpleSchedule
```

I personally add a simple function in .bashrc to run the program with a simple command sch.

```bash
function sch() { # use this function to run the program easily
    pushd /path/to/code # change this to the path of the code
    cabal run -- SimpleSchedule "$@"
    popd
}

function esch() { # use this function to edit the events easily
    nvim /path/to/code/app/MyEvents.hs # change this to the path of the code
}
```

which will produce output like this: (only events listed will show its information)

```plaintext

     9         10        11        12        13        14        15        16
6/30 .         .         .         .         .         .         .         .
7/1  :    [Example Event 1         :         :    ]    :         :    [ExamEven1]
7/2  :    [Example Event 1         :         :    ]    :         :    [ExamEven1]
7/3  :    [Example Event 1         :         :    ]    :         :    [ExamEven1]
7/4  :    [Example Event 1         :         :    ]    :         :    [ExamEven1]
7/5  :    [Example Event 1         :         :    ]    :         :    [ExamEven1]
7/6  .         .         .         .         .         .         .         .
7/7  .         .         .         .         .         .         .         .
7/8  :         :         :         :         :[SomeEverTwoWeekSemi]        :
7/9  :         :         :         :         :[SomeEverTwoWeekSemi]        :
7/10 :         :         :         :         :[SomeEverTwoWeekSemi]        :
7/11 :         :         :         :         :[SomeEverTwoWeekSemi]        :
7/12 :         :         :         :         :[SomeEverTwoWeekSemi]        :
7/13 .         .         .         .         .         .         .         .
7/14 .         .         .         .         .         .         .         .

Example Event 1
  @Example Location 1
  Example Event Website: https://example.com
  Useful reference: https://example.com/reference

Some Every Two Week Seminar
  @University of Someplace, Some Building, Some Room
  Seminar website: https://someplace.edu/seminar

```

You can run the program with a number parameter to specify the number of future days to show. Default is 14 days (changable in Main.hs). The width and style of the display is adjustable in the code (in TimeTable.hs).

This is a very simple program and you are welcomed to modify or improve it!

