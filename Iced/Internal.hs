module Iced.Internal where

import Foreign

foreign export ccall "free_stable_ptr"
  freeStablePtr :: StablePtr a -> IO ()

foreign export ccall "free_haskell_fun_ptr"
  freeHaskellFunPtr :: FunPtr a -> IO ()
