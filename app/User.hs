module User where

import Helpers
import System.Directory
import System.IO

register :: String -> String -> String -> IO ()
register username email password = do
  say ("getting file . . .") blue
  root <- getCurrentDirectory
  delay 2
  say ("creating data . . .") blue
  let newData = ("| " ++ username ++ " | " ++ email ++ " | " ++ (getPassword password) ++ " | " ++ "\n")
  delay 2
  say ("adding data . . .") blue
  appendFile (root ++ "/app/users.md") newData

updateUser :: (String, String, String) -> (String, String, String) -> IO ()
updateUser (oldUsername, oldEmail, oldPassword) (username, email, password) = do
  say ("getting file . . .") blue
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/users.md")
  delay 2
  say ("processing data . . .") blue
  let newData = (username ++ " | " ++ email ++ " | " ++ (getPassword password) ++ " | ")
  let userIdx = findUser oldUsername oldEmail oldPassword (lines content) (-1)
  let filtered = unlines (take userIdx (lines content) ++ [newData] ++ drop (userIdx + 1) (lines content))
  delay 2
  say ("updating file . . .") blue
  writeFile (root ++ "/app/users-copy.md") filtered
  removeFile (root ++ "/app/users.md")
  renameFile (root ++ "/app/users-copy.md") (root ++ "/app/users.md")
  delay 2
  say (email ++ " has been changed successfully from " ++ oldEmail ++ " ✔") green

deleteUser :: String -> String -> String -> IO ()
deleteUser username email password = do
  say ("getting file . . .") blue
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/users.md")
  delay 2
  say ("removing data . . .") blue
  let userIdx = findUser username email password (lines content) (-1)
  let filtered = unlines (take userIdx (lines content) ++ drop (userIdx + 1) (lines content))
  delay 2
  say ("updating file . . .") blue
  writeFile (root ++ "/app/users-copy.md") filtered
  removeFile (root ++ "/app/users.md")
  renameFile (root ++ "/app/users-copy.md") (root ++ "/app/users.md")
  delay 2
  say (email ++ " has been removed successfully ✔") green