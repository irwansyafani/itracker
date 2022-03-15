module Controllers where

import Helpers
import M

controller x = do
  if x == "1"
    then createAccount x
    else
      if x == "2"
        then updateAccount x
        else
          if x == "3"
            then deleteAccount x
            else
              if x == "4"
                then createCompanyProfile x
                else
                  if x == "5"
                    then checkTrackcer x
                    else
                      if x == "6"
                        then makeTracker x
                        else
                          if x == "7"
                            then updateTracker x
                            else
                              if x == "8"
                                then deleteTracker x
                                else
                                  if x == "9"
                                    then exportFileToCSV x
                                    else deleteCompanyProfile x

-- =======================================================================
-- ========================= F U N C T I O N S ===========================
-- =======================================================================

program = do
  showMenu
  say "What would you like to do? " cyan
  reqCommand <- getLine
  if (null reqCommand)
    then say "Thank you for using iSupply" blue
    else do
      if ((read reqCommand) `elem` [1 .. 10])
        then do controller reqCommand
        else putStrLn "\x1b[31mCommand Denied\x1b[0m"

-- ===========
-- = U S E R =
-- ===========
createAccount x = do
  mCreatingAccount
  program

updateAccount x = do
  mUpdatingAccount
  program

deleteAccount x = do
  mDeletingAccount
  program

-- ===========
-- = COMPANY =
-- ===========
createCompanyProfile x = do
  mCreateCompanyProfile
  program

deleteCompanyProfile x = do
  mDeleteCompanyProfile
  program

-- ===========
-- = TRACKER =
-- ===========
checkTrackcer x = do
  say "Moving to Check Tracker . . ." blue
  mGetTracker
  program

makeTracker x = do
  say "Moving to Make Tracker . . ." blue
  mMakeTracker
  program

updateTracker x = do
  say "Moving to Update Tracker . . ." blue
  mUpdateTracker
  program

deleteTracker x = do
  say "Moving to Delete Tracker . . ." blue
  mDeleteTracker
  program

-- ===========
-- == EXPORT =
-- ===========
exportFileToCSV x = do
  say "Moving to Export File to CSV . . ." blue
  filename <- prompt "Enter File Name"
  mCreateCSV filename
  program
