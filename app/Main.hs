module Main where

import Controllers
import Helpers
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  let uniqueID = (take 40 (randomRs ('a', 'z') gen) ++ "wans")
  welcoming
  program uniqueID
