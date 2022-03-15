module M where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT)
import Helpers
import Tracker

newtype Stack a = Stack {unStack :: StateT Int (WriterT [Int] IO) a}

creatingCSV :: String -> Stack ()
creatingCSV x = Stack $ do
  lift $ lift $ do createCSV x
  return ()

injection :: Stack a -> IO [Int]
injection m = execWriterT (evalStateT (unStack m) 0)

mCreateCSV :: String -> IO [Int]
mCreateCSV x = injection $ creatingCSV x
