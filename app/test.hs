-- TESTING PURPOSE

module Main where

import Control.Exception
import System.IO

main :: IO ()
main = getPassword >>= putStrLn . ("Entered: " ++)

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

prompt :: String -> IO String
prompt x = do
  putStrLn x
  b <- getLine
  return b

asdf = do
  pass <- prompt "Enter Password:"
  putStrLn pass