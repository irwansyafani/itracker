module Tracker where

import Data.List
import Helpers
import System.Directory

-- findone
getTrackerDetail :: IO ()
getTrackerDetail = do
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/activities.log")
  year <- prompt "Enter Year:"
  month <- prompt "Enter Month:"
  date <- prompt "Enter Date:"
  isSpesific <- prompt ("Do you want to set spesific time? " ++ reset ++ "(y/n)")
  if isInfixOf isSpesific "y"
    then do
      h <- prompt "Enter Hour:"
      m <- prompt "Enter Minute:"
      s <- prompt "Enter Seconds:"
      let time = ("[" ++ year ++ "-" ++ month ++ "-" ++ date ++ " " ++ h ++ ":" ++ m ++ ":" ++ s ++ "]")
      let filtered = findTrackerDetail time (lines content) (-1) []
      print filtered
    else do
      delay 1
      say "getting data . . ." blue
      -- let selected = findTracker code (lines content) (-1)
      print ((lines content) !! 0)

-- create
-- update
-- delete