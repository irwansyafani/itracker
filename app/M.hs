module M where

import Company
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT)
import Helpers
import Tracker
import User

newtype Stack a = Stack {unStack :: StateT Int (WriterT [Int] IO) a}

logging :: String -> Stack ()
logging x = Stack $ do
  lift $ lift $ do logger x
  return ()

creatingCSV :: String -> Stack ()
creatingCSV x = Stack $ do
  lift $ lift $ do createCSV x
  return ()

deletingTracker :: Stack ()
deletingTracker = Stack $ do
  lift $ lift $ do removeTracker
  return ()

updatingTracker :: Stack ()
updatingTracker = Stack $ do
  lift $ lift $ do editTracker
  return ()

makingTracker :: Stack ()
makingTracker = Stack $ do
  lift $ lift $ do createTracker
  return ()

gettingTracker :: Stack ()
gettingTracker = Stack $ do
  lift $ lift $ do getTrackerDetail
  return ()

deletingAccount :: Stack ()
deletingAccount = Stack $ do
  lift $
    lift $ do
      say "Moving to Delete Account . . ." blue
      username <- prompt "Enter Username:"
      email <- prompt "Enter Email:"
      password <- prompt "Enter Password:"
      deleteUser username email password
  return ()

updatingAccount :: Stack ()
updatingAccount = Stack $ do
  lift $
    lift $ do
      say "Moving to Update Account . . ." blue
      oldUsername <- prompt "Enter Old Username:"
      oldEmail <- prompt "Enter Old Email:"
      oldPassword <- prompt "Enter Old Password:"
      username <- prompt "Enter Username:"
      email <- prompt "Enter Email:"
      password <- prompt "Enter Password:"
      updateUser (oldUsername, oldEmail, oldPassword) (username, email, password)
  return ()

creatingAccount :: Stack ()
creatingAccount = Stack $ do
  lift $
    lift $ do
      say "Moving to Create Account . . ." blue
      username <- prompt "Enter Username:"
      email <- prompt "Enter Email:"
      password <- prompt "Enter Password:"
      register username email password
      say "Creating your account . . ." blue
      say "Account Successfully Created ✔" green
  return ()

deletingCompanyProfile :: Stack ()
deletingCompanyProfile = Stack $ do
  lift $
    lift $ do
      say "Moving to Delete Company Profile . . ." blue
      companyName <- prompt "Enter Name of Company:"
      companyAddress <- prompt "Enter Address of Company:"
      companyType <- prompt "Enter Business Type of Company:"
      companyNumber <- prompt "Enter Contact Number of Company:"
      picName <- prompt "Enter Name of PIC of Company:"
      picNumber <- prompt "Enter Contact Number of PIC of Company:"
      managerName <- prompt "Enter Name of Head Manager of Company:"
      removeCompany companyName companyAddress companyType companyNumber picName picNumber managerName
  return ()

creatingCompanyProfile :: Stack ()
creatingCompanyProfile = Stack $ do
  lift $
    lift $ do
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
  return ()

--
--
injection :: Stack a -> IO [Int]
injection m = execWriterT (evalStateT (unStack m) 0)

--
--
mCreateCSV :: String -> IO [Int]
mCreateCSV x = injection $ creatingCSV x

mDeleteTracker :: IO [Int]
mDeleteTracker = injection deletingTracker

mUpdateTracker :: IO [Int]
mUpdateTracker = injection updatingTracker

mMakeTracker :: IO [Int]
mMakeTracker = injection makingTracker

mGetTracker :: IO [Int]
mGetTracker = injection gettingTracker

mDeleteCompanyProfile :: IO [Int]
mDeleteCompanyProfile = injection deletingCompanyProfile

mCreateCompanyProfile :: IO [Int]
mCreateCompanyProfile = injection creatingCompanyProfile

mDeletingAccount :: IO [Int]
mDeletingAccount = injection deletingAccount

mUpdatingAccount :: IO [Int]
mUpdatingAccount = injection updatingAccount

mCreatingAccount :: IO [Int]
mCreatingAccount = injection creatingAccount

mLogger :: String -> IO [Int]
mLogger x = injection $ logging x
