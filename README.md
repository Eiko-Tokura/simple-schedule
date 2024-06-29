# simple-schedule

I use this simple schedule to keep track of my events because I keep forgetting them: the seminars and events are scattered around everywhere! And the functionalities provided by the usual calendars are extremely unweildy and painful to use. Therefore I decide to use a more flexible language to describe the events, which turns out to be much more convinient (at least for me). I hope this helps you too owo!

With this simple schedule program (which is highly modifiable), you can manage your generation of events using directly haskell code, which provides increadible expressiveness and flexibility. 

Modify the MyEvents.hs file to add your events, an example may look like this

```haskell
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
```

Then use cabal run or stack or ghc to complile and run the program. I personally add a simple function in .bashrc to run the program with a simple command sch.

```bash
function sch() {
    pushd /path/to/code
    cabal run -- SimpleSchedule "$@"
    popd
}
```

which will produce output like this:

```plaintext

     9         10        11        12        13        14        15        16
6/29 |         |         |         |         |         |         |         |
6/30 |         |         |         |         |         |         |         |
7/1  |    [Example Event 1         |         |    ]    |         |    [ExamEven1]
7/2  |    [Example Event 1         |         |    ]    |         |    [ExamEven1]
7/3  |    [Example Event 1         |         |    ]    |         |    [ExamEven1]
7/4  |    [Example Event 1         |         |    ]    |         |    [ExamEven1]
7/5  |    [Example Event 1         |         |    ]    |         |    [ExamEven1]
7/6  |         |         |         |         |         |         |         |
7/7  |         |         |         |         |         |         |         |
7/8  |         |         |         |         |[SomeEverTwoWeekSemi]        |
7/9  |         |         |         |         |[SomeEverTwoWeekSemi]        |
7/10 |         |         |         |         |[SomeEverTwoWeekSemi]        |
7/11 |         |         |         |         |[SomeEverTwoWeekSemi]        |
7/12 |         |         |         |         |[SomeEverTwoWeekSemi]        |
7/13 |         |         |         |         |         |         |         |

Example Event 1
  @Example Location 1
  Example Event 1: https://example.com
  Useful reference: https://example.com/reference

Some Every Two Week Seminar
  @University of Someplace, Some Building, Some Room
  Seminar website: https://someplace.edu/seminar

```

You can run the program with a number parameter to specify the number of future days to show. Default is 14 days. The width of the display is adjustable in the code.

This is a very simple program and you are welcomed to modify or improve it!

