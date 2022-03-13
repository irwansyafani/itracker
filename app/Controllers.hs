module Controllers where

import Company
import Helpers
import User
import Tracker

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
  say "Moving to Create Account . . ." blue
  username <- prompt "Enter Username:"
  email <- prompt "Enter Email:"
  password <- prompt "Enter Password:"
  register username email password
  say "Creating your account . . ." blue
  say "Account Successfully Created ✔" green
  program

updateAccount x = do
  say "Moving to Update Account . . ." blue
  oldUsername <- prompt "Enter Old Username:"
  oldEmail <- prompt "Enter Old Email:"
  oldPassword <- prompt "Enter Old Password:"
  username <- prompt "Enter Username:"
  email <- prompt "Enter Email:"
  password <- prompt "Enter Password:"
  updateUser (oldUsername, oldEmail, oldPassword) (username, email, password)
  program

deleteAccount x = do
  say "Moving to Delete Account . . ." blue
  username <- prompt "Enter Username:"
  email <- prompt "Enter Email:"
  password <- prompt "Enter Password:"
  deleteUser username email password
  program

-- ===========
-- = COMPANY =
-- ===========
createCompanyProfile x = do
  say "Moving to Create Company Profile . . ." blue
  say "" reset
  say "================================================" red
  say "THIS INFORMATION CAN NOT BE CHANGE AFTER CREATED" red
  say "            BE AWARE EVERY INFORMATION          " red
  say "             THAT YOU WANT TO SUBMIT            " red
  say "================================================" red
  say "================================================" red
  say "If there is information  that you want to change" red
  say "       you can exit the current process and     " red
  say "                 start the new one              " red
  say "================================================" red
  say "" reset
  delay 3
  companyName <- prompt "Enter Name of Company:"
  companyAddress <- prompt "Enter Address of Company:"
  companyType <- prompt "Enter Business Type of Company:"
  companyNumber <- prompt "Enter Contact Number of Company:"
  picName <- prompt "Enter Name of PIC of Company:"
  picNumber <- prompt "Enter Contact Number of PIC of Company:"
  managerName <- prompt "Enter Name of Head Manager of Company:"
  registerCompany companyName companyAddress companyType companyNumber picName picNumber managerName
  program

deleteCompanyProfile x = do
  say "Moving to Delete Company Profile . . ." blue
  companyName <- prompt "Enter Name of Company:"
  companyAddress <- prompt "Enter Address of Company:"
  companyType <- prompt "Enter Business Type of Company:"
  companyNumber <- prompt "Enter Contact Number of Company:"
  picName <- prompt "Enter Name of PIC of Company:"
  picNumber <- prompt "Enter Contact Number of PIC of Company:"
  managerName <- prompt "Enter Name of Head Manager of Company:"
  removeCompany companyName companyAddress companyType companyNumber picName picNumber managerName
  program

-- ===========
-- = TRACKER =
-- ===========
checkTrackcer x = do
  say "Moving to Check Tracker . . ." blue
  getTrackerDetail
  program

makeTracker x = do
  say "Moving to Make Tracker . . ." blue
  createTracker
  program

updateTracker x = do
  say "Moving to Update Tracker . . ." blue
  editTracker
  program

deleteTracker x = do
  say "Moving to Delete Tracker . . ." blue
  removeTracker
  program

-- ===========
-- == EXPORT =
-- ===========
exportFileToCSV x = do
  say "Moving to Export File to CSV . . ." blue
  filename <- prompt "Enter File Name"
  createCSV filename
  program