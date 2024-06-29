module Main where

import Control.Monad
import Data.Maybe
import Data.Time
import System.Environment
import Text.Read

import MyEvents
import TimeTable

main :: IO ()
main = do 
  args  <- getArgs -- specify the length of the timetable
  today <- utctDay <$> getCurrentTime
  let defaultDays = 14
      days  = fromMaybe defaultDays $ listToMaybe args >>= readMaybe
      intv  = (today, addDays days today)
      infos = showInfos myEvents intv
  unless (null infos) $ putStrLn ""
  putStrLn $ renderStringImage $ showTimeTable myEvents intv
  unless (null infos) $ putStrLn $ "\n" ++ infos
  unless (null infos) $ putStrLn ""

