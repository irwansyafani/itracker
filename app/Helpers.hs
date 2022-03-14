module Helpers where

import Control.Concurrent
import Data.List
import Data.List.Split
import Data.Time
import System.Directory

-- =======================================================================
-- ========================= V A R I A B L E S ===========================
-- =======================================================================
reset = "\x1b[0m"

black = "\x1b[30m"

red = "\x1b[31m"

green = "\x1b[32m"

yellow = "\x1b[33m"

blue = "\x1b[34m"

magenta = "\x1b[35m"

cyan = "\x1b[36m"

white = "\x1b[37m"

-- =======================================================================
-- ========================= F U N C T I O N S ===========================
-- =======================================================================
say x color =
  if (null color)
    then putStrLn ""
    else putStrLn (color ++ x ++ reset)

welcoming =
  do
    putStrLn cyan
    putStrLn ""
    putStrLn "           W E L C O  M E   T O   i S U P P L Y           "
    putStrLn reset

showMenu = do
  putStrLn "\x1b[36m"
  putStrLn "                      C O M M A N D S                     "
  putStrLn "     \x1b[33m1\x1b[36m> Create Account         \x1b[33m8\x1b[36m> Delete Tracker         "
  putStrLn "     \x1b[33m2\x1b[36m> Update Account         \x1b[33m9\x1b[36m> Export File to CSV     "
  putStrLn "     \x1b[33m3\x1b[36m> Delete Account         \x1b[33m10\x1b[36m> Delete Company Profile"
  putStrLn "     \x1b[33m4\x1b[36m> Create Company Profile \x1b[33mdefault\x1b[36m> EXIT            "
  putStrLn "     \x1b[33m5\x1b[36m> Check Tracker                                     "
  putStrLn "     \x1b[33m6\x1b[36m> Make A Tracker                                    "
  putStrLn "     \x1b[33m7\x1b[36m> Update Tracker                                    "
  putStrLn ""
  putStrLn "\x1b[0m"

prompt :: String -> IO String
prompt x = do
  putStrLn (cyan ++ x ++ reset)
  answer <- getLine
  if (null answer)
    then prompt x
    else return answer

promptChar :: String -> IO String
promptChar x = do
  putStrLn (cyan ++ x ++ reset)
  answer <- getLine
  if (null answer)
    then prompt x
    else return answer

-- splitString :: String -> Maybe [Char] -> String
-- splitString x null = splitString x " | "
splitString str = splitOn " | " str

splitLog str = splitOn "|" str

findUser :: String -> String -> String -> [String] -> Int -> Int
findUser username email password content counter
  | counter < 0 = findUser username email password content 0
  | (isInfixOf username  (splitLog (content !! counter) !! 1))
      && (isInfixOf email (splitLog (content !! counter) !! 2))
      && (isInfixOf password (splitLog (content !! counter) !! 3)) = counter
  | otherwise = findUser username email password content (counter + 1)

findCompanies :: (String, String, String, String, String, String, String) -> [String] -> Int -> Int
findCompanies (name, address, cType, number, picName, picNumber, manager) content counter
  | counter < 0 = findCompanies (name, address, cType, number, picName, picNumber, manager) content 0
  | (isInfixOf name (splitString (content !! counter) !! 0))
      && (isInfixOf address (splitString (content !! counter) !! 1))
      && (isInfixOf cType (splitString (content !! counter) !! 2))
      && (isInfixOf number (splitString (content !! counter) !! 3))
      && (isInfixOf picName (splitString (content !! counter) !! 4))
      && (isInfixOf picNumber (splitString (content !! counter) !! 5))
      && (isInfixOf manager (splitString (content !! counter) !! 6)) =
      counter
  | otherwise = findCompanies (name, address, cType, number, picName, picNumber, manager) content (counter + 1)

-- findTracker :: String -> String
findTracker code content counter
  | counter < 1 = findTracker code content 0
  | (code == (splitString (content !! counter) !! 0)) = (content !! counter)
  | otherwise = findTracker code content (counter + 1)

-- updateUser :: Int -> Int -> [String] -> String -> String -> String -> [String] -> IO ()
-- updateUser targetIdx counter (x : y : xs) username email password list
--   | counter < 0 = updateUser targetIdx 0 xs username email password list
--   | counter == targetIdx = do
--       let newData = (username ++ " | " ++ email ++ " | " ++ (getPassword password) ++ " | " ++ "\n")
--       updateUser targetIdx (counter + 1) ([x] ++ newData : xs) username email password list
--   | otherwise = updateUser targetIdx (counter + 1) (y:xs) username email password list

-- removeUser :: Int -> [String] -> Int -> [String] -> String
-- removeUser targetIdx (x : xs) counter list
--   | counter /= targetIdx = removeUser targetIdx xs (counter + 1) list

delay :: Int -> IO ()
delay x = threadDelay (x * 1000000)

convertUserToCSV :: [String] -> Int -> String -> String -> IO ()
convertUserToCSV content counter filename baseFile
  | counter == length content = say "successfully converting data to csv ✔" green
  | counter == 1 = convertUserToCSV content (counter + 1) filename baseFile
  | otherwise = do
      root <- getCurrentDirectory
      let currentData = (splitLog (content !! counter))
      if baseFile == "users"
        then
          appendFile
            (root ++ "/app/" ++ filename ++ ".csv")
            ( currentData !! 1 ++ ","
                ++ currentData !! 2
                ++ ","
                ++ currentData !! 3
                ++ "\n"
            )
        else
          appendFile
            (root ++ "/app/" ++ filename ++ ".csv")
            ( currentData !! 1 ++ ","
                ++ currentData !! 2
                ++ ","
                ++ currentData !! 3
                ++ ","
                ++ currentData !! 4
                ++ ","
                ++ currentData !! 5
                ++ ","
                ++ currentData !! 6
                ++ ","
                ++ currentData !! 7
                ++ "\n"
            )
      convertUserToCSV content (counter + 1) filename baseFile

createCSV filename = do
  say "creating file . . ." blue
  root <- getCurrentDirectory
  baseFile <- prompt "What file do you want to exported? (filename only)"
  content <- readFile (root ++ "/app/" ++ baseFile ++ ".md")
  writeFile (root ++ "/app/" ++ filename ++ ".csv") ""
  convertUserToCSV (lines content) 0 filename baseFile
  say ("file " ++ filename ++ ".csv created ✔ . . .") green

-- filename.csv
-- formatting to csv
--

findTrackerDetail :: String -> [String] -> Int -> String
findTrackerDetail time content counter
  | counter < 0 = findTrackerDetail time content 0
  | isInfixOf time (splitLog (content !! counter) !! 1) = (content !! counter)
  | otherwise = findTrackerDetail time content (counter + 1)

-- getDate :: String
-- getDate = formatTime defaultTimeLocale "[%Y-%m-%d %T]" getCurrentTime

getFlag :: String -> String
getFlag x =
  if x == "D"
    then "<span style=\"color: lightgreen\">DEBUG</span>"
    else
      if x == "I"
        then "<span style=\"color: lightblue\">INFO</span>"
        else
          if x == "W"
            then "<span style=\"color: orange\">WARN</span> "
            else "<span style=\"color: red\">ERROR</span> "

findOneTracker :: String -> [String] -> Int -> Int
findOneTracker x content counter
  | counter < 0 = findOneTracker x content 0
  | isInfixOf x (content !! counter) = counter
  | otherwise = findOneTracker x content (counter + 1)

getPassword :: String -> String
getPassword x = x
