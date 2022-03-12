module MonadConstructor where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, runState)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)

newtype Stack a = Stack {unStack :: StateT Int (WriterT [Int] IO) a}