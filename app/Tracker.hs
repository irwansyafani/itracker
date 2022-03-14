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
  trxId <- prompt "Enter Transaction ID:"
  say "getting data . . ." blue
  delay 1
  let filtered = findTrackerDetail trxId (lines content) (-1)
  delay 1
  let fetched = splitOn "|" filtered
  say "---" blue
  say ("Transaction ID    :" ++ fetched !! 1) green
  say ("code              :" ++ fetched !! 2) green
  say ("status            :" ++ fetched !! 3) green
  say ("product           :" ++ fetched !! 4) green
  say ("quantity          :" ++ fetched !! 5) green
  say ("shipper           :" ++ fetched !! 6) green
  say ("message           :" ++ fetched !! 7) green
  say "---" blue
  delay 2

createTracker :: IO ()
createTracker = do
  root <- getCurrentDirectory
  flg <- promptChar "Enter Status: (I, O, E, D)"
  code <- prompt "Enter Company Code:"
  product <- prompt "Enter Product Name:"
  qty <- prompt "Enter Quantity:"
  shipper <- prompt "Enter Shipper:"
  msg <- prompt "Enter Message:"
  trxId <- prompt "Enter Transaction ID:"
  delay 2
  say "Processing Data" blue
  let newData = ("| " ++ trxId ++ " " ++ "| " ++ code ++ " | " ++ (getFlag flg) ++ " | " ++ product ++ " | " ++ qty ++ " | " ++ shipper ++ " | " ++ msg ++ " |\n")
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
  trxId <- prompt "Enter Transaction ID:"
  msg <- prompt "Enter New Message:"
  say "processing data . . ." blue
  delay 1
  let trackerIdx = findOneTracker trxId (lines content) (-1)
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
  trxId <- prompt "Enter Transaction ID:"
  let trackerIdx = findOneTracker trxId (lines content) (-1)
  let filtered = (unlines ((take trackerIdx (lines content)) ++ drop (trackerIdx + 1) (lines content)))
  delay 2
  say ("updating file . . .") blue
  writeFile (root ++ "/app/tracker-copy.md") filtered
  removeFile (root ++ "/app/tracker.md")
  renameFile (root ++ "/app/tracker-copy.md") (root ++ "/app/tracker.md")
  delay 1
  say "data successfully removed ✔" green
  delay 2