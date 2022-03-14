module Company where

import Helpers
import System.Directory

registerCompany companyName companyAddress companyType companyNumber picName picNumber managerName = do
  root <- getCurrentDirectory
  delay 1
  say "inserting data . . ." blue
  appendFile
    (root ++ "/app/companies.md")
    ( "\n"
        ++ "| "
        ++ companyName
        ++ " | "
        ++ companyAddress
        ++ " | "
        ++ companyType
        ++ " | "
        ++ companyNumber
        ++ " | "
        ++ picName
        ++ " | "
        ++ picNumber
        ++ " | "
        ++ managerName
        ++ " |"
    )
  delay 1
  say "inserting data success" blue
  delay 1
  say "companies.md is up to date ✔" green

removeCompany companyName companyAddress companyType companyNumber picName picNumber managerName = do
  say ("getting file . . .") blue
  root <- getCurrentDirectory
  content <- readFile (root ++ "/app/companies.md")
  delay 2
  say ("removing data . . .") blue
  let companyId = findCompanies (companyName, companyAddress, companyType, companyNumber, picName, picNumber, managerName) (lines content) (-1)
  let filtered = unlines (take companyId (lines content) ++ drop (companyId + 1) (lines content))
  delay 2
  say ("updating file . . .") blue
  writeFile (root ++ "/app/companies-copy.md") filtered
  removeFile (root ++ "/app/companies.md")
  renameFile (root ++ "/app/companies-copy.md") (root ++ "/app/companies.md")
  delay 2
  say (companyName ++ " has been removed ✔") green