module Iced.Future.Runtime (run) where

import Foreign (FunPtr)

import Iced.Future.Internal

foreign import ccall "future_run"
  future_run :: FunPtr a -> IO ()

type MainCallback = IO (FuturePtr ())

foreign import ccall "wrapper"
  makeMainCallback :: MainCallback -> IO (FunPtr MainCallback)

run :: Future () -> IO ()
run (Future callback) = future_run =<< makeMainCallback callback
