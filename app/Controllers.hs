module Controllers where

import Helpers
import M

controller x id = do
  if x == "1"
    then createAccount id
    else
      if x == "2"
        then updateAccount id
        else
          if x == "3"
            then deleteAccount id
            else
              if x == "4"
                then createCompanyProfile id
                else
                  if x == "5"
                    then checkTrackcer id
                    else
                      if x == "6"
                        then makeTracker id
                        else
                          if x == "7"
                            then updateTracker id
                            else
                              if x == "8"
                                then deleteTracker id
                                else
                                  if x == "9"
                                    then exportFileToCSV id
                                    else deleteCompanyProfile id

-- =======================================================================
-- ========================= F U N C T I O N S ===========================
-- =======================================================================

-- program :: String -> IO String
program id = do
  showMenu
  say "What would you like to do? " cyan
  reqCommand <- getLine
  if (null reqCommand)
    then do say "Thank you for using iTracker" blue
    else do
      if ((read reqCommand) `elem` [1 .. 10])
        then do controller reqCommand id
        else do say "Command Denied" red

-- ===========
-- = U S E R =
-- ===========
createAccount uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "INFO -- : admin is creating account")
  mCreatingAccount
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin is successfully create account")
  program uniqueID

updateAccount uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "WARN -- : admin is updating account")
  mUpdatingAccount
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin is successfully update account")
  program uniqueID

deleteAccount uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "WARN -- : admin is removing account")
  mDeletingAccount
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin is remove account")
  program uniqueID

-- ===========
-- = COMPANY =
-- ===========
createCompanyProfile uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "INFO -- : admin is creating company profile")
  mCreateCompanyProfile
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin is successfully created company profile")
  program uniqueID

deleteCompanyProfile uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "WARN -- : admin is removing company profile")
  mDeleteCompanyProfile
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin is successfully removed company profile")
  program uniqueID

-- ===========
-- = TRACKER =
-- ===========
checkTrackcer uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "DEBUG -- : admin is checking a tracker")
  say "Moving to Check Tracker . . ." blue
  mGetTracker
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin was checked a tracker")
  program uniqueID

makeTracker uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "INFO -- : admin is creating a tracker")
  say "Moving to Make Tracker . . ." blue
  mMakeTracker
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin was create a tracker")
  program uniqueID

updateTracker uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "WARN -- : admin is updating a tracker")
  say "Moving to Update Tracker . . ." blue
  mUpdateTracker
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin was update a tracker")
  program uniqueID

deleteTracker uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "WARN -- : admin is removing a tracker")
  say "Moving to Delete Tracker . . ." blue
  mDeleteTracker
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin was removed a tracker")
  program uniqueID

-- ===========
-- == EXPORT =
-- ===========
exportFileToCSV uniqueID = do
  mLogger ("[" ++ uniqueID ++ "] " ++ "INFO -- : admin is exporting a file")
  say "Moving to Export File to CSV . . ." blue
  filename <- prompt "Enter File Name"
  mCreateCSV filename
  mLogger ("[" ++ uniqueID ++ "] " ++ "SUCCESS -- : admin was exported a file")
  program uniqueID
