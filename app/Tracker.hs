module Tracker where

import Data.List
import Data.List.Split
import Helpers
import System.Directory

-- findone
getTrackerDetail :: IO ()
getTrackerDetail = do
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/tracker.md")
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
      let time = ("[" ++ year ++ "-" ++ month ++ "-" ++ date)
      let selected = findTracker time (lines content) (-1)
      print ((lines content) !! 0)

createTracker :: IO ()
createTracker = do
  root <- getCurrentDirectory
  flg <- promptChar "Enter Status: (I, O, E, D)"
  code <- prompt "Enter Company Code:"
  product <- prompt "Enter Product Name:"
  qty <- prompt "Enter Quantity:"
  shipper <- prompt "Enter Shipper:"
  msg <- prompt "Enter Message:"
  delay 2
  say "Processing Data" blue
  let newData = ("| 2022-03-02 06:52:50 " ++ "| " ++ code ++ " | " ++ (getFlag flg) ++ " | " ++ product ++ " | " ++ qty ++ " | " ++ shipper ++ " | " ++ msg ++ " |\n")
  -- let data = ("| 2022-03-02 06:52:50 " ++ "| " ++ code ++ " | " ++ flag ++ " | " ++ product ++ " | " ++ qty ++ " | " ++ shipper ++ " | " ++ msg ++ " |")
  delay 1
  say "Updating File" blue
  appendFile (root ++ "/app/tracker.md") newData
  delay 2
  say "Tracker Successfully Created ✔" green
  delay 1

-- update
editTracker :: IO ()
editTracker = do
  say "getting file . . ." blue
  delay 1
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/tracker.md")
  y <- prompt "Enter Year:"
  m <- prompt "Enter Month:"
  d <- prompt "Enter Date:"
  h <- prompt "Enter Hour:"
  mi <- prompt "Enter Minute:"
  s <- prompt "Enter Seconds:"
  msg <- prompt "Enter New Message:"
  say "processing data . . ." blue
  delay 1
  let date = (y ++ "-" ++ m ++ "-" ++ d ++ " " ++ h ++ ":" ++ mi ++ ":" ++ s)
  let trackerIdx = findOneTracker date (lines content) (-1)
  let selected = splitOn "|" ((lines content) !! trackerIdx)
  let newData = intercalate "|" (take 7 selected ++ [(" " ++ msg ++ " "), ""])
  let filtered = (unlines ((take trackerIdx (lines content)) ++ [newData] ++ drop (trackerIdx + 1) (lines content)))
  say "updating file . . ." blue
  writeFile (root ++ "/app/tracker-copy.md") filtered
  removeFile (root ++ "/app/tracker.md")
  renameFile (root ++ "/app/tracker-copy.md") (root ++ "/app/tracker.md")
  delay 1
  say "tracker successfully updated ✔" green
  delay 1

-- delete
removeTracker :: IO ()
removeTracker = do
  say "getting file . . ." blue
  delay 1
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/tracker.md")
  delay 1
  y <- prompt "Enter Year:"
  m <- prompt "Enter Month:"
  d <- prompt "Enter Date:"
  h <- prompt "Enter Hour:"
  mi <- prompt "Enter Minute:"
  s <- prompt "Enter Seconds:"
  -- code <- prompt "Enter Company Code:"
  -- flg <- promptChar "Enter Status: (I, O, E, D)"
  -- product <- prompt "Enter Product Name:"
  -- qty <- prompt "Enter Quantity:"
  -- shipper <- prompt "Enter Shipper:"
  -- msg <- prompt "Enter Message:"
  let date = (y ++ "-" ++ m ++ "-" ++ d ++ " " ++ h ++ ":" ++ mi ++ ":" ++ s)
  let trackerIdx = findOneTracker date (lines content) (-1)
  let filtered = (unlines ((take trackerIdx (lines content)) ++ drop (trackerIdx + 1) (lines content)))
  delay 2
  say ("updating file . . .") blue
  writeFile (root ++ "/app/tracker-copy.md") filtered
  removeFile (root ++ "/app/tracker.md")
  renameFile (root ++ "/app/tracker-copy.md") (root ++ "/app/tracker.md")
  delay 1
  say "data successfully removed ✔" green
  delay 2